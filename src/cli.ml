type emit = Ir | Ir_json | Llvm | Optimized_llvm | Asm | Obj | Header | Executable
type diagnostics = Text | Json

type t = {
  input : string;
  output : string;
  output_explicit : bool;
  emit : emit;
  keep : bool;
  optimization : int;
  debug : bool;
  no_inline_function : string option;
  sanitizers : string list;
  link_inputs : string list;
  c_flags : string list;
  diagnostics : diagnostics;
  target_cpu : string option;
}

type perf_options = {
  top : int;
  static : bool;
  json : bool;
  asm : (string * int) option;
  copy : int option;
  compare : string option;
}

type command =
  | Run of t
  | Run_program of t * string list
  | Check of t
  | Perf of t * perf_options * string list
  | Help
  | Version

type subcommand = Build | Run_subcommand | Check_subcommand | Perf_subcommand

let default_perf =
  { top = 5; static = false; json = false; asm = None; copy = None; compare = None }

let usage =
  "usage: fas [options] file.fas [C inputs and link flags]\n\
  \       fas run file.fas [options] [-- program arguments]\n\
  \       fas check file.fas [options]\n\
  \       fas perf file.fas [options] [--top N] [--static] [--compare FILE] [-- \
   program arguments]\n\
   commands:\n\
  \  run FILE      build to a temporary executable, run it, delete it\n\
  \  check FILE    check without building (no LLVM, no linking)\n\
  \  perf FILE     profile: hot loops by Fas line (needs perf)\n\
   build:\n\
  \  -O0..-O3      set opt and llc optimization levels (default -O2)\n\
   output:\n\
  \  -o PATH       output path (default a.out, INPUT.o with -c, INPUT.s with -S)\n\
  \  --emit-header write an ABI-checked C header (stdout without -o)\n\
  \  --emit-llvm[=optimized]  print LLVM IR before (default) or after opt, verified\n\
  \  --emit-asm, -S emit assembly\n\
  \  --emit-obj, -c emit object\n\
  \  --diagnostics=text|json  print diagnostics as text (default) or one JSON line\n\
  \  --version     print the fas version and the opt, llc and C compiler in use\n\
  \  -h, --help    show this help\n\
   C inputs:\n\
  \  -I DIR, -isystem DIR, -D NAME[=VALUE] (attached or separate)\n\
  \  -l, -L        link flags (attached or separate)\n\
  \  FILE.c, FILE.o, FILE.a, FILE.so  C inputs linked with the program\n\
   debugging:\n\
  \  --emit-ir     print the compiler's custom IR dump\n\
  \  --emit-ir-json print the custom IR as JSON\n\
  \  --keep        retain and report intermediate paths and active tool pipeline\n\
  \  -g            emit DWARF line tables (machine code unchanged)\n\
  \  --no-inline NAME  add LLVM noinline to NAME\n\
  \  --sanitize=LIST instrument with address and/or undefined behavior sanitizers\n\
   environment:\n\
  \  FAS_OPT, FAS_LLC, FAS_CC select opt, llc and the C compiler (defaults: opt-22, \
   llc-22, clang-22)\n\
  \  FAS_OPT_PASSES overrides opt's pipeline (default: default<Olevel>)"

let default_output emit input =
  let source_output extension =
    Filename.remove_extension (Filename.basename input) ^ extension
  in
  match emit with Asm -> source_output ".s" | Obj -> source_output ".o" | _ -> "a.out"

let parse_invocation ~subcommand argv =
  let n = Array.length argv in
  let header_selected = ref false and other_output = ref false in
  let link_inputs_rev = ref [] in
  let c_flags_rev = ref [] in
  let sanitizers = ref None in
  let diagnostics = ref Text in
  let add_link_inputs values =
    link_inputs_rev := List.rev_append values !link_inputs_rev
  in
  let add_c_flags values = c_flags_rev := List.rev_append values !c_flags_rev in
  let passthrough_file file =
    List.exists (Filename.check_suffix file) [ ".c"; ".o"; ".a"; ".so" ]
  in
  let rejected flag =
    match subcommand with
    | Build -> false
    | Run_subcommand | Perf_subcommand ->
        List.mem flag [ "-o"; "-c"; "-S" ] || String.starts_with ~prefix:"--emit-" flag
    | Check_subcommand ->
        List.mem flag [ "-o"; "-c"; "-S"; "-l"; "-L" ]
        || String.starts_with ~prefix:"--emit-" flag
        || String.starts_with ~prefix:"-l" flag
        || String.starts_with ~prefix:"-L" flag
        || passthrough_file flag
  in
  let rec loop i input output emit keep optimization optimization_explicit debug
      no_inline_function =
    if i >= n then
      if Option.is_none input then Error "no input files"
      else if !header_selected && !other_output then
        Error "--emit-header cannot be combined with other output modes"
      else if !link_inputs_rev <> [] && emit <> Executable then
        Error "C inputs and link flags require an executable output"
      else
        let input = Option.get input in
        let output_explicit = Option.is_some output in
        let output =
          match output with Some path -> path | None -> default_output emit input
        in
        let config =
          {
            input;
            output;
            output_explicit;
            emit;
            keep;
            optimization;
            debug;
            no_inline_function;
            sanitizers = Option.value ~default:[] !sanitizers;
            link_inputs = List.rev !link_inputs_rev;
            c_flags = List.rev !c_flags_rev;
            diagnostics = !diagnostics;
            target_cpu = None;
          }
        in
        Ok (if subcommand = Check_subcommand then Check config else Run config)
    else
      match argv.(i) with
      | "--help" | "-h" -> Ok Help
      | flag when rejected flag ->
          Error
            (match subcommand with
            | Check_subcommand ->
                "fas check checks one Fas file; `" ^ flag ^ "` is not accepted"
            | Perf_subcommand ->
                "fas perf builds and profiles an executable; `" ^ flag
                ^ "` is not accepted"
            | Build | Run_subcommand ->
                "fas run builds and runs an executable; `" ^ flag ^ "` is not accepted")
      | "-o" ->
          if i + 1 >= n then Error "-o requires an output path"
          else if argv.(i + 1) = "-" then Error "-o - is not supported"
          else
            loop (i + 2) input
              (Some argv.(i + 1))
              emit keep optimization optimization_explicit debug no_inline_function
      | "--emit-header" ->
          header_selected := true;
          loop (i + 1) input output Header keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-ir" ->
          other_output := true;
          loop (i + 1) input output Ir keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-ir-json" ->
          other_output := true;
          loop (i + 1) input output Ir_json keep optimization optimization_explicit
            debug no_inline_function
      | "--emit-llvm" ->
          other_output := true;
          loop (i + 1) input output Llvm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-llvm=optimized" ->
          other_output := true;
          loop (i + 1) input output Optimized_llvm keep optimization
            optimization_explicit debug no_inline_function
      | flag when String.starts_with ~prefix:"--emit-llvm=" flag ->
          let value = String.sub flag 12 (String.length flag - 12) in
          Error
            ("unknown --emit-llvm value `" ^ value ^ "`; accepted value is `optimized`")
      | "--emit-asm" | "-S" ->
          other_output := true;
          loop (i + 1) input output Asm keep optimization optimization_explicit debug
            no_inline_function
      | "--emit-obj" | "-c" ->
          other_output := true;
          loop (i + 1) input output Obj keep optimization optimization_explicit debug
            no_inline_function
      | flag when String.starts_with ~prefix:"--diagnostics=" flag -> (
          let value = String.sub flag 14 (String.length flag - 14) in
          match value with
          | "text" ->
              diagnostics := Text;
              loop (i + 1) input output emit keep optimization optimization_explicit
                debug no_inline_function
          | "json" ->
              diagnostics := Json;
              loop (i + 1) input output emit keep optimization optimization_explicit
                debug no_inline_function
          | _ ->
              Error
                ("unknown --diagnostics value `" ^ value
               ^ "`; accepted values are `text` and `json`"))
      | "--keep" ->
          loop (i + 1) input output emit true optimization optimization_explicit debug
            no_inline_function
      | flag when String.starts_with ~prefix:"--sanitize=" flag -> (
          let value = String.sub flag 11 (String.length flag - 11) in
          let names = String.split_on_char ',' value in
          if value = "" then Error "--sanitize requires a non-empty list"
          else
            let unknown =
              List.find_opt (fun name -> name <> "address" && name <> "undefined") names
            in
            match unknown with
            | Some name -> Error ("unknown sanitizer `" ^ name ^ "`")
            | None -> (
                let names = List.sort_uniq String.compare names in
                match !sanitizers with
                | Some previous when previous <> names ->
                    Error "conflicting --sanitize options"
                | _ ->
                    sanitizers := Some names;
                    loop (i + 1) input output emit keep optimization
                      optimization_explicit debug no_inline_function))
      | "-g" ->
          loop (i + 1) input output emit keep optimization optimization_explicit true
            no_inline_function
      | "--no-inline" ->
          if i + 1 >= n then Error "--no-inline requires a function name"
          else
            let name = argv.(i + 1) in
            if name = "" || name.[0] = '-' then
              Error "--no-inline requires a function name"
            else if Option.is_some no_inline_function then
              Error "duplicate --no-inline option"
            else
              loop (i + 2) input output emit keep optimization optimization_explicit
                debug (Some name)
      | ("-l" | "-L") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            add_link_inputs [ flag; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug
              no_inline_function)
      | ("-I" | "-isystem" | "-D") as flag ->
          if i + 1 >= n || argv.(i + 1) = "" then Error (flag ^ " requires an argument")
          else (
            add_c_flags [ flag; argv.(i + 1) ];
            loop (i + 2) input output emit keep optimization optimization_explicit debug
              no_inline_function)
      | flag
        when (String.length flag > 2 && String.sub flag 0 2 = "-l")
             || (String.length flag > 2 && String.sub flag 0 2 = "-L") ->
          add_link_inputs [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | flag when String.starts_with ~prefix:"-isystem" flag && String.length flag > 8
        ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | flag when String.starts_with ~prefix:"-I" flag && String.length flag > 2 ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | flag when String.starts_with ~prefix:"-D" flag && String.length flag > 2 ->
          add_c_flags [ flag ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | flag
        when String.length flag = 3
             && flag.[0] = '-'
             && flag.[1] = 'O'
             && flag.[2] >= '0'
             && flag.[2] <= '3' ->
          loop (i + 1) input output emit keep
            (Char.code flag.[2] - Char.code '0')
            true debug no_inline_function
      | "-debug" -> Error "unknown option `-debug`; write `-g`"
      | "-no-inline" -> Error "unknown option `-no-inline`; write `--no-inline`"
      | flag when String.length flag > 0 && flag.[0] = '-' ->
          Error ("unknown option: " ^ flag)
      | file when passthrough_file file ->
          add_link_inputs [ file ];
          loop (i + 1) input output emit keep optimization optimization_explicit debug
            no_inline_function
      | file -> (
          match input with
          | None ->
              loop (i + 1) (Some file) output emit keep optimization
                optimization_explicit debug no_inline_function
          | Some _ ->
              Error
                "multiple input files are not supported; use \"path.fas\" for \
                 dependencies")
  in
  loop
    (if subcommand = Build then 1 else 2)
    None None
    (if subcommand = Check_subcommand then Ir else Executable)
    false 2 false false None

let split_perf_options argv =
  let n = Array.length argv in
  let rec scan i kept options =
    if i >= n then Ok (Array.of_list (List.rev kept), options)
    else
      let with_value error update =
        if i + 1 >= n then Error error
        else
          match update argv.(i + 1) with
          | Some options -> scan (i + 2) kept options
          | None -> Error error
      in
      match argv.(i) with
      | "--static" -> scan (i + 1) kept { options with static = true }
      | "--json" -> scan (i + 1) kept { options with json = true }
      | "--compare" ->
          with_value "--compare requires a file" (fun path ->
              Some { options with compare = Some path })
      | "--top" ->
          with_value "--top requires a number from 1 to 50" (fun text ->
              match int_of_string_opt text with
              | Some top when top >= 1 && top <= 50 -> Some { options with top }
              | _ -> None)
      | "--asm" ->
          with_value "--asm requires FILE:LINE or FILE:LINE@N" (fun text ->
              let at =
                Option.value (String.rindex_opt text '@') ~default:(String.length text)
              in
              let copy =
                if at = String.length text then Some None
                else
                  match
                    int_of_string_opt
                      (String.sub text (at + 1) (String.length text - at - 1))
                  with
                  | Some n when n >= 1 -> Some (Some n)
                  | _ -> None
              in
              let place = String.sub text 0 at in
              match (String.rindex_opt place ':', copy) with
              | Some colon, Some copy -> (
                  let line =
                    String.sub place (colon + 1) (String.length place - colon - 1)
                  in
                  match int_of_string_opt line with
                  | Some line when colon > 0 && line > 0 ->
                      Some
                        {
                          options with
                          asm = Some (String.sub place 0 colon, line);
                          copy;
                        }
                  | _ -> None)
              | _ -> None)
      | flag -> scan (i + 1) (flag :: kept) options
  in
  scan 0 [] default_perf

let perf_command config options args =
  if options.json && Option.is_some options.asm then
    Error "fas perf: --json and --asm cannot be combined"
  else if options.static && args <> [] then
    Error "fas perf --static does not run the program; `--` arguments are not accepted"
  else Ok (Perf (config, options, args))

let parse argv =
  let n = Array.length argv in
  let subcommand =
    if n > 1 && String.equal argv.(1) "run" then Run_subcommand
    else if n > 1 && String.equal argv.(1) "check" then Check_subcommand
    else if n > 1 && String.equal argv.(1) "perf" then Perf_subcommand
    else Build
  in
  let rec find i = if i >= n || String.equal argv.(i) "--" then i else find (i + 1) in
  let sep = if subcommand = Build || subcommand = Check_subcommand then n else find 2 in
  if n > 1 && Array.exists (String.equal "--version") (Array.sub argv 1 (sep - 1)) then
    if n = 2 then Ok Version
    else Error "--version cannot be combined with other arguments"
  else
    let args =
      match Array.to_list (Array.sub argv sep (n - sep)) with
      | _ :: args -> args
      | [] -> []
    in
    let build_argv = Array.sub argv 0 sep in
    let invocation =
      if subcommand = Perf_subcommand then split_perf_options build_argv
      else Ok (build_argv, default_perf)
    in
    match invocation with
    | Error _ as error -> error
    | Ok (build_argv, options) -> (
        match parse_invocation ~subcommand build_argv with
        | Ok (Run config) when subcommand = Run_subcommand ->
            Ok (Run_program (config, args))
        | Ok (Run config) when subcommand = Perf_subcommand ->
            perf_command config options args
        | result -> result)
