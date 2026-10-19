let ( let* ) = Result.bind

type candidate = {
  name : string;
  choice : Driver.choice;
  counts : string;
  input : string;
}

type placement = { layout : int option; offset : int option }

type run = {
  wall : float;
  cycles : float option;
  status : string;
  stdout : string;
  stderr : string;
}

type session = {
  runner : string;
  arena : string;
  cpu : int;
  dir : string;
  log : string option;
  argv0 : string;
  args : string list;
}

exception Stop of int

let rounds = 7
let layouts = [ None; Some 32; Some 64 ]
let offsets = [ 0; 65536; 589824 ]

let placements no_arena =
  List.concat_map
    (fun layout ->
      if no_arena then [ { layout; offset = None } ]
      else List.map (fun offset -> { layout; offset = Some offset }) offsets)
    layouts

let placement_name p =
  Driver.align_loops_name p.layout
  ^ match p.offset with Some n -> "+" ^ string_of_int n | None -> ""

let read path = In_channel.with_open_bin path In_channel.input_all

let rec remove_tree path =
  match Sys.is_directory path with
  | true ->
      Array.iter
        (fun name -> remove_tree (Filename.concat path name))
        (Sys.readdir path);
      Sys.rmdir path
  | false -> Sys.remove path
  | exception Sys_error _ -> ()

let mkdir path = if not (Sys.file_exists path) then Sys.mkdir path 0o700

let parse_cpu_list text =
  String.split_on_char ',' (String.trim text)
  |> List.concat_map (fun part ->
      match String.split_on_char '-' part with
      | [ a ] -> Option.to_list (int_of_string_opt a)
      | [ a; b ] -> (
          match (int_of_string_opt a, int_of_string_opt b) with
          | Some a, Some b when a <= b -> List.init (b - a + 1) (fun i -> a + i)
          | _ -> [])
      | _ -> [])

let allowed_cpus () =
  match
    read "/proc/self/status" |> String.split_on_char '\n'
    |> List.find_map (fun line ->
        if String.starts_with ~prefix:"Cpus_allowed_list:" line then
          Some (String.sub line 18 (String.length line - 18))
        else None)
  with
  | Some list -> parse_cpu_list list
  | None | (exception Sys_error _) -> []

let cpu_times () =
  match read "/proc/stat" with
  | exception Sys_error _ -> []
  | text ->
      String.split_on_char '\n' text
      |> List.filter_map (fun line ->
          match String.split_on_char ' ' line |> List.filter (( <> ) "") with
          | name :: fields
            when String.length name > 3 && String.starts_with ~prefix:"cpu" name -> (
              match
                ( int_of_string_opt (String.sub name 3 (String.length name - 3)),
                  List.map int_of_string_opt fields )
              with
              | Some cpu, values when List.for_all Option.is_some values ->
                  let values = List.map Option.get values in
                  let total = List.fold_left ( + ) 0 values in
                  let idle =
                    match values with
                    | _ :: _ :: _ :: idle :: iowait :: _ -> idle + iowait
                    | _ -> 0
                  in
                  Some (cpu, (total, idle))
              | _ -> None)
          | _ -> None)

let siblings cpu =
  match
    read
      (Printf.sprintf "/sys/devices/system/cpu/cpu%d/topology/thread_siblings_list" cpu)
  with
  | text -> ( match parse_cpu_list text with [] -> [ cpu ] | list -> list)
  | exception Sys_error _ -> [ cpu ]

let choose_cpu () =
  let allowed = allowed_cpus () in
  let isolated =
    match read "/sys/devices/system/cpu/isolated" with
    | text -> parse_cpu_list text
    | exception Sys_error _ -> []
  in
  match List.find_opt (fun cpu -> List.mem cpu allowed) isolated with
  | Some cpu -> cpu
  | None -> (
      let before = cpu_times () in
      Unix.sleepf 0.2;
      let after = cpu_times () in
      let busy cpu =
        match (List.assoc_opt cpu before, List.assoc_opt cpu after) with
        | Some (t0, i0), Some (t1, i1) -> t1 - t0 - (i1 - i0)
        | _ -> max_int / 4
      in
      let cores =
        List.filter
          (fun cpu ->
            let group = siblings cpu in
            List.hd group = cpu && List.for_all (fun c -> List.mem c allowed) group)
          allowed
      in
      let score cpu = List.fold_left (fun acc c -> acc + busy c) 0 (siblings cpu) in
      match List.sort (fun a b -> compare (score a, a) (score b, b)) cores with
      | cpu :: _ -> cpu
      | [] -> ( match allowed with cpu :: _ -> cpu | [] -> -1))

let log_line session text =
  match session.log with
  | None -> ()
  | Some path ->
      Out_channel.with_open_gen [ Open_append; Open_creat ] 0o644 path (fun channel ->
          output_string channel (text ^ "\n"))

let run_counter = ref 0

let execute session ?(env = Unix.environment ()) ?(arena = true) ~label placement
    program =
  incr run_counter;
  let out = Filename.concat session.dir (Printf.sprintf "run-%d" !run_counter) in
  mkdir out;
  let arena_path, offset =
    match (arena, placement) with
    | true, Some { offset = Some offset; _ } -> (session.arena, string_of_int offset)
    | _ -> ("-", "0")
  in
  log_line session label;
  let argv =
    Array.of_list
      ([
         session.runner;
         string_of_int session.cpu;
         arena_path;
         offset;
         out;
         program;
         "--";
         session.argv0;
       ]
      @ session.args)
  in
  let failed message =
    prerr_endline ("fas: cannot run the tune runner: " ^ message);
    raise (Stop 1)
  in
  (match
     Unix.create_process_env session.runner argv env Unix.stdin Unix.stdout Unix.stderr
   with
  | exception Unix.Unix_error (code, _, _) -> failed (Unix.error_message code)
  | pid -> (
      let rec wait () =
        match Unix.waitpid [] pid with
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> wait ()
        | _, status -> status
      in
      match wait () with Unix.WEXITED 0 -> () | _ -> failed "it did not finish"));
  let result = read (Filename.concat out "result") in
  let fields = String.split_on_char ' ' (String.trim result) in
  let run =
    match fields with
    | [ wall; cycles; _; status; _ ] ->
        {
          wall = float_of_string wall;
          cycles = float_of_string_opt cycles;
          status;
          stdout = read (Filename.concat out "stdout");
          stderr = read (Filename.concat out "stderr");
        }
    | _ -> failed ("malformed result " ^ result)
  in
  remove_tree out;
  run

let same a b = a.status = b.status && a.stdout = b.stdout && a.stderr = b.stderr

let status_message = function
  | "0" -> None
  | status when String.starts_with ~prefix:"sig:" status ->
      Some
        (Printf.sprintf "error: the program was killed by signal %s; no profile written"
           (String.sub status 4 (String.length status - 4)))
  | status ->
      Some
        (Printf.sprintf "error: the program exited with status %s; no profile written"
           status)

let build ~report (config : Cli.t) ?profile ?(instrument = false)
    ?(target = config.target) ~input output =
  match
    Driver.run ~instrument
      {
        config with
        Cli.input;
        output;
        output_explicit = true;
        profile;
        target;
        emit = Cli.Executable;
      }
  with
  | Ok _ -> ()
  | Error diagnostics ->
      report diagnostics;
      raise (Stop 1)

let train ~report session config ~target name =
  let dir = Filename.concat session.dir ("train-" ^ name) in
  mkdir dir;
  let program = Filename.concat dir "program" in
  build ~report config ~instrument:true ~target ~input:config.Cli.input program;
  let env =
    Array.of_list
      (("LLVM_PROFILE_FILE=" ^ Filename.concat dir "fas-%p-%m.profraw")
      :: (Array.to_list (Unix.environment ())
         |> List.filter (fun entry ->
             not (String.starts_with ~prefix:"LLVM_PROFILE_FILE=" entry))))
  in
  let run = execute session ~env ~arena:false ~label:("train " ^ name) None program in
  (match status_message run.status with
  | Some message ->
      prerr_string run.stderr;
      prerr_endline message;
      raise (Stop 1)
  | None -> ());
  let profraw =
    Sys.readdir dir |> Array.to_list
    |> List.filter (fun name -> Filename.check_suffix name ".profraw")
    |> List.sort String.compare
    |> List.map (Filename.concat dir)
  in
  if profraw = [] then (
    prerr_endline
      "error: the program wrote no profile (it must return from `main` or call `exit`)";
    raise (Stop 1));
  match Driver.merge_profraw ~scratch:dir ~profraw with
  | Ok counts -> counts
  | Error diagnostics ->
      report diagnostics;
      raise (Stop 1)

type measured = { comparison : Tune_decide.comparison; index : int }

let profile_text ~header ~choice ~layout ~measured ~counts =
  String.concat "\n"
    (header
    @ [
        "choice " ^ Driver.choice_name choice;
        "align-loops " ^ Driver.align_loops_name layout;
        "measured " ^ measured;
        "counts";
      ])
  ^ "\n" ^ counts

let write ~report output text =
  match Driver.write_profile_file output text with
  | Ok () -> ()
  | Error diagnostics ->
      report diagnostics;
      raise (Stop 1)

let percent band = Printf.sprintf "%.1f%%" (band *. 100.)

let tune ~report (config : Cli.t) ~output ~no_arena ~dir ~argv0 args =
  let cc = Driver.tool "FAS_CC" "clang-22" in
  let tools = Filename.concat dir "tools" in
  mkdir tools;
  let runner, arena =
    match Tune_tools.build ~cc tools with
    | Ok tools -> tools
    | Error message ->
        prerr_endline ("fas: " ^ message);
        raise (Stop 1)
  in
  let session =
    {
      runner;
      arena;
      cpu = choose_cpu ();
      dir;
      log = Sys.getenv_opt "FAS_TUNE_LOG";
      argv0;
      args;
    }
  in
  let header = Driver.tune_header () in
  let native_only = config.target = Cli.Force_native in
  let first_counts =
    train ~report session config ~target:config.target
      (if native_only then "native" else "portable")
  in
  let native_counts =
    if native_only then first_counts
    else train ~report session config ~target:Cli.Force_native "native"
  in
  let test_input = Sys.getenv_opt "FAS_TUNE_TEST_CANDIDATE" in
  let candidate name choice counts index =
    {
      name;
      choice;
      counts;
      input =
        (match test_input with Some input when index = 1 -> input | _ -> config.input);
    }
  in
  let candidates =
    Array.of_list
      (if native_only then
         [
           candidate "default" Driver.Native native_counts 0;
           candidate "pgo" Driver.Native_pgo native_counts 1;
         ]
       else
         [
           candidate "default" Driver.Default first_counts 0;
           candidate "pgo" Driver.Pgo first_counts 1;
           candidate "native" Driver.Native first_counts 2;
           candidate "native-pgo" Driver.Native_pgo native_counts 3;
         ])
  in
  let placements = Array.of_list (placements no_arena) in
  let builds = Filename.concat dir "builds" in
  mkdir builds;
  let program c layout =
    Filename.concat builds (c.name ^ "-" ^ Driver.align_loops_name layout)
  in
  let unmeasured = "none" in
  Array.iter
    (fun c ->
      List.iter
        (fun layout ->
          let profile = program c layout ^ ".fasprof" in
          write ~report profile
            (profile_text ~header ~choice:c.choice ~layout ~measured:unmeasured
               ~counts:c.counts);
          build ~report config ~profile ~target:Cli.Portable ~input:c.input
            (program c layout))
        layouts)
    candidates;
  let baseline = candidates.(0) in
  let suffix = "" in
  let default_profile reason =
    prerr_endline ("fas perf: " ^ reason ^ "; the profile keeps the default build");
    write ~report output
      (profile_text ~header ~choice:baseline.choice ~layout:None ~measured:unmeasured
         ~counts:first_counts);
    Printf.eprintf "fas perf: wrote %s\n%!" output;
    raise (Stop 0)
  in
  let reference =
    let run () =
      execute session ~arena:false
        ~label:("reference " ^ baseline.name)
        None (program baseline None)
    in
    let first = run () in
    let second = run () in
    if not (same first second) then
      default_profile "the program's output differs between two runs of the same build";
    first
  in
  let check index p run =
    if not (same reference run) then
      if index = 0 then
        match p.offset with
        | Some offset ->
            prerr_endline
              (Printf.sprintf
                 "error: the program behaves differently under the measuring allocator \
                  (arena offset %d); rerun with --no-arena"
                 offset);
            raise (Stop 1)
        | None ->
            default_profile
              "the program's output differs between two runs of the same build"
      else (
        prerr_endline
          (Printf.sprintf
             "internal error: a tuned build changed the program's output (%s, %s); \
              kept builds in %s"
             candidates.(index).name
             (match p.offset with
             | Some n -> "arena offset " ^ string_of_int n
             | None -> "no arena")
             builds);
        raise (Stop 2))
  in
  let metric run = match run.cycles with Some c when c > 0. -> c | _ -> run.wall in
  let scales =
    match Sys.getenv_opt "FAS_TUNE_TEST_SCALE" with
    | None -> []
    | Some text ->
        String.split_on_char ',' text
        |> List.filter_map (fun item ->
            match String.split_on_char '=' item with
            | [ name; factor ] ->
                Option.map (fun factor -> (name, factor)) (float_of_string_opt factor)
            | _ -> None)
  in
  let wall_metric = ref false in
  let measure_once phase index p =
    let c = candidates.(index) in
    let run =
      execute session
        ~label:(Printf.sprintf "%s %s %s" phase c.name (placement_name p))
        (Some p) (program c p.layout)
    in
    check index p run;
    (match run.cycles with Some c when c > 0. -> () | _ -> wall_metric := true);
    metric run *. Option.value ~default:1. (List.assoc_opt c.name scales)
  in
  let measure phase indices =
    let pairs =
      Array.of_list
        (List.concat_map
           (fun index -> List.init (Array.length placements) (fun p -> (index, p)))
           indices)
    in
    let samples = Hashtbl.create 64 in
    for round = 0 to rounds - 1 do
      let n = Array.length pairs in
      for k = 0 to n - 1 do
        let index, p = pairs.((round + k) mod n) in
        let value = measure_once phase index placements.(p) in
        Hashtbl.replace samples (index, p)
          (value :: Option.value ~default:[] (Hashtbl.find_opt samples (index, p)))
      done
    done;
    fun index ->
      Array.init (Array.length placements) (fun p ->
          Tune_decide.summarize (Array.of_list (Hashtbl.find samples (index, p))))
  in
  let all = List.init (Array.length candidates) Fun.id in
  List.iter
    (fun index ->
      Array.iter (fun p -> ignore (measure_once "check" index p)) placements)
    all;
  let selection = measure "select" all in
  let others = Array.init (Array.length candidates - 1) (fun i -> selection (i + 1)) in
  let decision =
    match Tune_decide.select ~rounds ~baseline:(selection 0) ~candidates:others with
    | Tune_decide.Selected (i, _) ->
        let index = i + 1 in
        let confirmation = measure "confirm" [ 0; index ] in
        let comparison =
          Tune_decide.confirm ~rounds ~baseline:(confirmation 0)
            ~candidate:(confirmation index)
        in
        Some ({ comparison; index }, confirmation)
    | Tune_decide.Rejected best ->
        Option.map
          (fun (i, comparison) -> ({ comparison; index = i + 1 }, selection))
          best
  in
  if !wall_metric then
    prerr_endline "fas perf: cycle counters are not available; measured wall time";
  let metric_name = if !wall_metric then "wall" else "cycles" in
  Printf.eprintf "fas perf: measured on CPU %d, %d rounds\n" session.cpu rounds;
  let by_layout (data : Tune_decide.side array) =
    Array.of_list
      (List.map
         (fun layout ->
           Array.of_list
             (List.filter_map Fun.id
                (List.mapi
                   (fun p placement ->
                     if placement.layout = layout then Some data.(p) else None)
                   (Array.to_list placements))))
         layouts)
  in
  let kept, layout, band =
    match decision with
    | None -> (baseline, None, 0.)
    | Some (m, data) ->
        let shown = candidates.(m.index).name in
        Printf.eprintf "%-14s %14s %14s %8s\n" "placement" baseline.name shown "ratio";
        let base = data 0 and cand = data m.index in
        Array.iteri
          (fun p placement ->
            Printf.eprintf "%-14s %14.0f %14.0f %8.3f\n" (placement_name placement)
              base.(p).median cand.(p).median m.comparison.ratios.(p))
          placements;
        let c = m.comparison in
        if c.keep then (
          let layout =
            match Tune_decide.best_layout ~rounds ~layouts:(by_layout cand) with
            | Some i -> List.nth layouts i
            | None -> None
          in
          Printf.eprintf "fas perf: kept the profile (%s %.3f×, band %s%s)%s\n" shown
            c.geomean (percent c.band)
            (match layout with
            | Some n -> Printf.sprintf ", loops aligned to %d" n
            | None -> "")
            suffix;
          (candidates.(m.index), layout, c.band))
        else (
          Printf.eprintf "fas perf: kept the default build (best %s %.3f×, band %s)%s\n"
            shown c.geomean (percent c.band) suffix;
          (baseline, None, c.band))
  in
  if decision = None then Printf.eprintf "fas perf: kept the default build%s\n" suffix;
  let ratio =
    match decision with
    | Some (m, _) when kept != baseline -> m.comparison.geomean
    | _ -> 1.
  in
  (match kept.choice with
  | Driver.Native | Driver.Native_pgo ->
      Printf.eprintf "fas perf: tuned for %s; the binary may not run on other CPUs\n"
        (Driver.llc_host_cpu ())
  | Driver.Default | Driver.Pgo -> ());
  let measured =
    Printf.sprintf "%.4f band %.4f rounds %d placements %d metric %s" ratio band rounds
      (Array.length placements) metric_name
  in
  write ~report output
    (profile_text ~header ~choice:kept.choice ~layout ~measured ~counts:kept.counts);
  Printf.eprintf "fas perf: wrote %s (%d functions)\n%!" output
    (Driver.count_profile_functions kept.counts);
  0

let run ~report (config : Cli.t) ~output ~no_arena ~argv0 args =
  match Driver.check_clang () with
  | Error diagnostics ->
      report diagnostics;
      1
  | Ok () -> (
      match Sys.getenv_opt "FAS_TUNE_TEST_TOOLS" with
      | Some dir -> (
          match Tune_tools.build ~cc:(Driver.tool "FAS_CC" "clang-22") dir with
          | Ok _ -> 0
          | Error message ->
              prerr_endline ("fas: " ^ message);
              1)
      | None -> (
          match Filename.temp_dir "fas-tune-" "" with
          | exception Sys_error message ->
              prerr_endline ("fas: " ^ message);
              1
          | dir ->
              let status =
                try tune ~report config ~output ~no_arena ~dir ~argv0 args
                with Stop status -> status
              in
              if not (config.keep || status = 2) then remove_tree dir;
              if status = 2 then 1 else status))
