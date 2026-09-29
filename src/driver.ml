let read_file path =
  try
    if Sys.file_exists path && Sys.is_directory path then
      Error (path ^ ": is a directory")
    else
      let channel = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () ->
          let length = in_channel_length channel in
          Ok (really_input_string channel length))
  with Sys_error message -> Error message

let normalize_absolute path =
  let parts = String.split_on_char '/' path in
  let parts =
    List.fold_left
      (fun acc part ->
        match part with
        | "" | "." -> acc
        | ".." -> ( match acc with [] -> [] | _ :: rest -> rest)
        | _ -> part :: acc)
      [] parts
    |> List.rev
  in
  "/" ^ String.concat "/" parts

let rec canonical_path path =
  try Unix.realpath path
  with Unix.Unix_error _ ->
    let absolute =
      if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path else path
    in
    let parent = Filename.dirname absolute in
    if parent = absolute then normalize_absolute absolute
    else
      normalize_absolute
        (Filename.concat (canonical_path parent) (Filename.basename absolute))

let use_path_error path =
  if String.contains path '\000' then
    Some "Fas dependency paths cannot contain NUL bytes"
  else if not (Filename.is_relative path) then
    Some
      "absolute Fas dependency paths are not supported; use a path relative to this \
       file"
  else if not (Filename.check_suffix path ".fas") then
    Some
      "Fas dependency paths must end in lowercase `.fas`; C headers use `use \"C\"` in \
       v0.2"
  else None

let include_chain_note paths = "include chain: " ^ String.concat " -> " paths

let with_chain diagnostics paths =
  List.map
    (fun (diagnostic : Diag.t) ->
      { diagnostic with Diag.notes = diagnostic.notes @ [ include_chain_note paths ] })
    diagnostics

let definition_file note =
  let prefix = "first definition is at " in
  let prefix_length = String.length prefix in
  if String.length note < prefix_length || String.sub note 0 prefix_length <> prefix
  then None
  else
    let location = String.sub note prefix_length (String.length note - prefix_length) in
    try
      let last = String.rindex location ':' in
      let before_column = String.sub location 0 last in
      let line = String.rindex before_column ':' in
      Some (String.sub before_column 0 line)
    with Not_found -> None

let add_include_chains chains diagnostics =
  let chain_for file = List.assoc_opt file chains in
  List.map
    (fun (diagnostic : Diag.t) ->
      let files =
        diagnostic.primary.Span.file :: List.filter_map definition_file diagnostic.notes
      in
      let notes =
        List.filter_map
          (fun file ->
            Option.map (fun chain -> include_chain_note chain) (chain_for file))
          files
        |> List.sort_uniq String.compare
      in
      { diagnostic with Diag.notes = diagnostic.notes @ notes })
    diagnostics

let load_program ~limits root =
  let loaded = Hashtbl.create 16 in
  let chains = Hashtbl.create 16 in
  let total_bytes = ref 0 in
  let budget_error span name value =
    Error
      [
        Diag.error span
          (Printf.sprintf "dependency closure exceeds budget %s of %d (profile %s)" name
             value
             (Limits.budget_profile_name limits));
      ]
  in
  let rec visit chain path use_span =
    let canonical = canonical_path path in
    if Hashtbl.mem loaded canonical then Ok ()
    else
      let file_chain = chain @ [ canonical ] in
      let primary = Option.value use_span ~default:Span.synthetic in
      if limits.Limits.max_use_files < 0 then
        Error
          [
            Diag.error primary
              (Printf.sprintf "budget max_use_files must not be negative (profile %s)"
                 (Limits.budget_profile_name limits));
          ]
      else if Hashtbl.length loaded >= limits.Limits.max_use_files then
        budget_error primary "max_use_files" limits.Limits.max_use_files
      else if limits.Limits.max_use_bytes < 0 then
        Error
          [
            Diag.error primary
              (Printf.sprintf "budget max_use_bytes must not be negative (profile %s)"
                 (Limits.budget_profile_name limits));
          ]
      else
        match Unix.stat canonical with
        | stats when stats.Unix.st_kind = Unix.S_DIR ->
            let diagnostics =
              [ Diag.error primary ("Fas dependency is a directory: " ^ canonical) ]
            in
            if Option.is_some use_span then Error (with_chain diagnostics file_chain)
            else Error diagnostics
        | stats when stats.Unix.st_size > limits.Limits.max_use_bytes - !total_bytes ->
            budget_error primary "max_use_bytes" limits.Limits.max_use_bytes
        | _ -> (
            match read_file canonical with
            | Error message ->
                let diagnostics =
                  [ Diag.error primary ("cannot read Fas dependency: " ^ message) ]
                in
                if Option.is_some use_span then
                  Error (with_chain diagnostics file_chain)
                else Error diagnostics
            | Ok text -> (
                if String.length text > limits.Limits.max_use_bytes - !total_bytes then
                  budget_error primary "max_use_bytes" limits.Limits.max_use_bytes
                else
                  let source = Source.create ~file:canonical ~text in
                  match Parser.parse ~limits source with
                  | Error diagnostics -> Error (with_chain diagnostics file_chain)
                  | Ok program ->
                      total_bytes := !total_bytes + String.length text;
                      Hashtbl.add loaded canonical program;
                      Hashtbl.add chains canonical file_chain;
                      let rec dependencies = function
                        | [] -> Ok ()
                        | Ast.Use { path = dependency; c_header = None; span } :: rest
                          -> (
                            match use_path_error dependency with
                            | Some message ->
                                Error
                                  (with_chain
                                     [ Diag.error span message ]
                                     (file_chain @ [ dependency ]))
                            | None -> (
                                let target =
                                  Filename.concat (Filename.dirname canonical)
                                    dependency
                                in
                                let target_canonical = canonical_path target in
                                match visit file_chain target_canonical (Some span) with
                                | Error diagnostics -> Error diagnostics
                                | Ok () -> dependencies rest))
                        | Ast.Use { c_header = Some _; _ } :: rest -> dependencies rest
                        | _ :: rest -> dependencies rest
                      in
                      dependencies program.Ast.items))
        | exception Unix.Unix_error (error, _, _) ->
            let message = Unix.error_message error in
            let diagnostics =
              [
                Diag.error primary
                  ("cannot read Fas dependency " ^ canonical ^ ": " ^ message);
              ]
            in
            if Option.is_some use_span then Error (with_chain diagnostics file_chain)
            else Error diagnostics
  in
  match visit [] root None with
  | Error diagnostics -> Error diagnostics
  | Ok () ->
      let files =
        Hashtbl.fold (fun path program acc -> (path, program) :: acc) loaded []
        |> List.sort (fun (left, _) (right, _) -> String.compare left right)
      in
      let items =
        List.concat_map
          (fun (_, program) ->
            List.filter (function Ast.Use _ -> false | _ -> true) program.Ast.items)
          files
      in
      let c_imports =
        List.filter_map
          (fun (path, program) ->
            let headers =
              List.filter_map
                (function
                  | Ast.Use { c_header = Some spelling; span; _ } ->
                      Some C_import.{ spelling; span }
                  | _ -> None)
                program.Ast.items
            in
            if headers = [] then None else Some (path, headers))
          files
      in
      let chains =
        Hashtbl.fold (fun path chain acc -> (path, chain) :: acc) chains []
      in
      Ok ({ Ast.items }, List.map (fun (path, chain) -> (path, chain)) chains, c_imports)

let write_file path text =
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel text)

let emit_text config text =
  if config.Cli.output_explicit then
    try
      write_file config.output text;
      Ok ""
    with Sys_error message ->
      Error [ Diag.error Span.synthetic ("output I/O failed: " ^ message) ]
  else Ok text

let tool name legacy default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> (
      match Sys.getenv_opt legacy with
      | Some value when value <> "" -> value
      | _ -> default)

let tools () =
  ( tool "LLVM_OPT" "FAS_OPT" "opt-22",
    tool "LLVM_LLC" "FAS_LLC" "llc-22",
    tool "CC" "FAS_CC" "clang-22" )

let status_name = function
  | Unix.WEXITED code -> Printf.sprintf "exit %d" code
  | Unix.WSIGNALED signal -> Printf.sprintf "signal %d" signal
  | Unix.WSTOPPED signal -> Printf.sprintf "stopped by signal %d" signal

let tool_error label failure =
  let output =
    if failure.Process.stderr <> "" then failure.stderr
    else if failure.stdout <> "" then failure.stdout
    else "no tool output"
  in
  Diag.error Span.synthetic
    (Printf.sprintf "%s failed (%s): %s" label (status_name failure.status)
       (String.trim output))

let ( let* ) result next =
  match result with Ok value -> next value | Error diagnostics -> Error diagnostics

let limits = Limits.default

let ast_budget result =
  match result with Ok () -> Ok () | Error diagnostic -> Error [ diagnostic ]

let ir_budget program result =
  match result with
  | Ok () -> Ok ()
  | Error (offender, message) ->
      let span =
        match offender with
        | Some name -> (
            match Ast.item_span_by_name program name with
            | Some span -> span
            | None -> Span.synthetic)
        | None -> Span.synthetic
      in
      Error [ Diag.error span message ]

let run_tool label argv =
  match Process.run argv with
  | Ok _ -> Ok ()
  | Error failure -> Error [ tool_error label failure ]

let remove path = try Sys.remove path with Sys_error _ -> ()
let optimization_level level = max 0 (min 3 level)

let render_ir ir =
  match Ir.render_bounded ~budget:limits.Limits.max_rendered_ir_bytes ir with
  | Ok text -> Ok text
  | Error message -> Error [ Diag.error Span.synthetic message ]

let opt_pass level =
  match Sys.getenv_opt "FAS_OPT_PASSES" with
  | Some value when value <> "" -> value
  | _ -> Printf.sprintf "default<O%d>" (optimization_level level)

let llc_opt level = Printf.sprintf "-O%d" (optimization_level level)

let verify_llvm opt file =
  run_tool
    ("LLVM verification with " ^ opt)
    [| opt; "-passes=verify"; file; "-disable-output" |]

let pass_report level =
  match Sys.getenv_opt "FAS_OPT_PASSES" with
  | Some value when value <> "" -> value
  | _ -> Printf.sprintf "default<O%d>" (optimization_level level)

let report_kept config paths opt llc cc =
  if config.Cli.keep then (
    prerr_endline ("fas: kept intermediates: " ^ String.concat ", " paths);
    prerr_endline
      (Printf.sprintf
         "fas: tools LLVM_OPT=%s LLVM_LLC=%s CC=%s; opt passes=%s; verify before and \
          after opt; llc %s"
         opt llc cc
         (pass_report config.Cli.optimization)
         (llc_opt config.Cli.optimization)))

let executable_command config cc asm_path c_objects =
  let argv =
    Array.of_list
      (cc :: asm_path
      :: (c_objects @ config.Cli.c_flags @ config.Cli.link_inputs
        @ [ "-o"; config.Cli.output ]))
  in
  if config.Cli.debug || config.Cli.keep then
    prerr_endline ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
  argv

let run_llc config llc ~filetype ~input ~output =
  run_tool llc
    [|
      llc;
      llc_opt config.Cli.optimization;
      "-relocation-model=pic";
      "-filetype=" ^ filetype;
      input;
      "-o";
      output;
    |]

let build_assembly config ir llc opt_path asm_path =
  let* () = run_llc config llc ~filetype:"asm" ~input:opt_path ~output:asm_path in
  let* generated =
    read_file asm_path
    |> Result.map_error (fun message ->
        [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ])
  in
  let assembly = generated ^ Ir.raw_assembly ir in
  write_file asm_path assembly;
  Ok assembly

let emit_tools_unprotected config program ir c_objects =
  let* () = ir_budget program (Ir.check_static_data_bytes ~limits ir) in
  let* () = ir_budget program (Ir.check_raw_asm_bytes ~limits ir) in
  let* ll_text = render_ir ir in
  let ll_path = Filename.temp_file "fas-module-" ".ll" in
  let opt_path = Filename.temp_file "fas-opt-" ".ll" in
  let asm_path = Filename.temp_file "fas-module-" ".s" in
  let cleanup () =
    if not config.Cli.keep then List.iter remove [ ll_path; opt_path; asm_path ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      write_file ll_path ll_text;
      let opt, llc, cc = tools () in
      report_kept config [ ll_path; opt_path; asm_path ] opt llc cc;
      let* () = verify_llvm opt ll_path in
      let* () =
        run_tool opt
          [|
            opt;
            "-passes=" ^ opt_pass config.optimization;
            "-verify-each";
            ll_path;
            "-S";
            "-o";
            opt_path;
          |]
      in
      let* () = verify_llvm opt opt_path in
      match config.emit with
      | Cli.Asm ->
          let* assembly = build_assembly config ir llc opt_path asm_path in
          write_file config.output assembly;
          Ok ""
      | Cli.Obj when Ir.raw_assembly ir = "" ->
          let* () =
            if c_objects = [] then
              run_llc config llc ~filetype:"obj" ~input:opt_path ~output:config.output
            else
              let fas_object = Filename.temp_file "fas-object-" ".o" in
              let cleanup_object () = if not config.Cli.keep then remove fas_object in
              Fun.protect ~finally:cleanup_object (fun () ->
                  if config.Cli.keep then
                    prerr_endline ("fas: kept Fas object: " ^ fas_object);
                  let* () =
                    run_llc config llc ~filetype:"obj" ~input:opt_path
                      ~output:fas_object
                  in
                  let argv =
                    Array.of_list
                      ((cc :: "-r" :: fas_object :: c_objects) @ [ "-o"; config.output ])
                  in
                  if config.Cli.debug || config.Cli.keep then
                    prerr_endline
                      ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
                  run_tool cc argv)
          in
          Ok ""
      | Cli.Obj ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let fas_object =
            if c_objects = [] then config.output
            else Filename.temp_file "fas-object-" ".o"
          in
          let cleanup_object () =
            if c_objects <> [] && not config.Cli.keep then remove fas_object
          in
          let* () =
            Fun.protect ~finally:cleanup_object (fun () ->
                if config.Cli.keep && c_objects <> [] then
                  prerr_endline ("fas: kept Fas object: " ^ fas_object);
                let argv = [| cc; "-c"; asm_path; "-o"; fas_object |] in
                if config.Cli.debug || config.Cli.keep then
                  prerr_endline
                    ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
                let* () = run_tool cc argv in
                if c_objects = [] then Ok ()
                else
                  let argv =
                    Array.of_list
                      ((cc :: "-r" :: fas_object :: c_objects) @ [ "-o"; config.output ])
                  in
                  if config.Cli.debug || config.Cli.keep then
                    prerr_endline
                      ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
                  run_tool cc argv)
          in
          Ok ""
      | Cli.Executable ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let* () = run_tool cc (executable_command config cc asm_path c_objects) in
          Ok ""
      | Cli.Ir | Cli.Llvm -> invalid_arg "Driver.emit_tools: non-tool emission")

let emit_tools config program ir c_objects =
  try emit_tools_unprotected config program ir c_objects with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]

let apply_no_inline config ir =
  match config.Cli.no_inline_function with
  | None -> Ok ir
  | Some name -> (
      match List.find_opt (fun (f : Ir.func) -> f.name = name) ir.Ir.funcs with
      | None ->
          Error
            [
              Diag.error Span.synthetic
                (Printf.sprintf "-no-inline function `%s` was not emitted" name);
            ]
      | Some f when f.blocks = [] || Option.is_some f.asm_body ->
          Error
            [
              Diag.error Span.synthetic
                (Printf.sprintf "-no-inline function `%s` is not a normal definition"
                   name);
            ]
      | Some _ -> Ok { ir with Ir.no_inline_function = Some name })

type imported_c_unit = {
  source : string;
  unit_path : string;
  use_span : Span.t;
  has_fragments : bool;
  static_functions : C_import.static_function list;
  c_names : string list;
}

let compile_c_units config cc units adapters artifacts =
  let rec compile acc = function
    | [] -> Ok (List.rev acc)
    | unit :: rest -> (
        let has_adapters =
          List.exists (fun adapter -> adapter.C_import.file = unit.source) adapters
        in
        if (not unit.has_fragments) && not has_adapters then compile acc rest
        else
          let object_path = Filename.temp_file "fas-c-object-" ".o" in
          let argv =
            Array.of_list
              ([
                 cc;
                 "--target=x86_64-unknown-linux-gnu";
                 "-fPIC";
                 llc_opt config.Cli.optimization;
               ]
              @ config.Cli.c_flags
              @ [ "-c"; unit.unit_path; "-o"; object_path ])
          in
          if config.Cli.debug || config.Cli.keep then
            prerr_endline ("fas: CC command: " ^ String.concat " " (Array.to_list argv));
          match Process.run argv with
          | Ok _ ->
              artifacts := object_path :: !artifacts;
              if config.Cli.keep then
                prerr_endline ("fas: kept C object: " ^ object_path);
              compile (object_path :: acc) rest
          | Error failure ->
              remove object_path;
              let message =
                Option.value ~default:(String.trim failure.stderr)
                  (C_import.first_error failure.stderr)
              in
              Error
                [
                  Diag.error
                    (C_import.mapped_error_span unit.source unit.use_span failure.stderr)
                    (if message = "" then "Clang C compilation failed"
                     else "Clang C compilation failed: " ^ message);
                ])
  in
  compile [] units

let add_static_adapters units ir =
  let called = Hashtbl.create 32 in
  let occupied = Hashtbl.create 64 in
  List.iter
    (fun unit -> List.iter (fun name -> Hashtbl.replace occupied name ()) unit.c_names)
    units;
  List.iter (fun (func : Ir.func) -> Hashtbl.replace occupied func.name ()) ir.Ir.funcs;
  List.iter
    (function
      | Ir.String_global { name; _ }
      | Ir.Array_global { name; _ }
      | Ir.Storage_global { name; _ } ->
          Hashtbl.replace occupied name ())
    ir.Ir.globals;
  List.iter
    (fun (func : Ir.func) ->
      List.iter
        (fun (block : Ir.block) ->
          List.iter
            (function
              | Ir.Call (_, _, _, name, _) -> Hashtbl.replace called name () | _ -> ())
            block.instrs)
        func.blocks)
    ir.Ir.funcs;
  let all_adapters = ref [] in
  let rec collect = function
    | [] -> Ok (List.rev !all_adapters)
    | unit :: rest ->
        let rec make = function
          | [] -> collect rest
          | static :: tail when Hashtbl.mem called static.C_import.name -> (
              match
                C_import.make_adapter
                  ~occupied:
                    (Hashtbl.fold (fun name _ names -> name :: names) occupied [])
                  unit.source static
              with
              | Ok adapter ->
                  all_adapters := adapter :: !all_adapters;
                  Hashtbl.replace occupied adapter.symbol ();
                  make tail
              | Error message -> Error [ Diag.error static.span message ])
          | _ :: tail -> make tail
        in
        make unit.static_functions
  in
  let* adapters = collect units in
  List.iter
    (fun unit ->
      let adapters =
        List.filter (fun adapter -> adapter.C_import.file = unit.source) adapters
      in
      if adapters <> [] then C_import.append_adapters unit.unit_path adapters)
    units;
  let redirects = Hashtbl.create (List.length adapters) in
  List.iter
    (fun (adapter : C_import.adapter) ->
      Hashtbl.replace redirects adapter.c_name adapter.symbol)
    adapters;
  let redirect name = Option.value ~default:name (Hashtbl.find_opt redirects name) in
  let funcs =
    List.map
      (fun (func : Ir.func) ->
        let blocks =
          List.map
            (fun (block : Ir.block) ->
              let instrs =
                List.map
                  (function
                    | Ir.Call (result, extension, ty, name, args) ->
                        Ir.Call (result, extension, ty, redirect name, args)
                    | instruction -> instruction)
                  block.instrs
              in
              { block with Ir.instrs })
            func.blocks
        in
        { func with Ir.name = redirect func.name; blocks })
      ir.Ir.funcs
  in
  let ir = { ir with Ir.funcs } in
  match Ir.validate ir with
  | Ok () -> Ok (ir, adapters)
  | Error message -> Error [ Diag.error Span.synthetic ("internal error: " ^ message) ]

let run_unprotected config =
  let c_artifacts = ref [] in
  Fun.protect
    ~finally:(fun () -> if not config.Cli.keep then List.iter remove !c_artifacts)
    (fun () ->
      match load_program ~limits config.Cli.input with
      | Error diagnostics -> Error diagnostics
      | Ok (program, chains, c_imports) -> (
          let _, _, cc = tools () in
          let imported = ref [] in
          let c_units = ref [] in
          let* () =
            let rec import = function
              | [] -> Ok ()
              | (source, headers) :: rest ->
                  let* declarations, kept, artifacts =
                    C_import.import ~cc ~debug:config.Cli.debug ~keep:config.Cli.keep
                      ~retain:true ~c_flags:config.Cli.c_flags source headers
                  in
                  c_artifacts := artifacts @ !c_artifacts;
                  let mapped =
                    C_import.map_declarations ~span:(List.hd headers).C_import.span
                      declarations
                  in
                  c_units :=
                    {
                      source;
                      unit_path = List.hd artifacts;
                      use_span = (List.hd headers).C_import.span;
                      has_fragments =
                        List.exists
                          (fun header ->
                            match header.C_import.spelling with
                            | Ast.C_fragment _ -> true
                            | Ast.C_quoted _ | Ast.C_system _ -> false)
                          headers;
                      static_functions = mapped.static_functions;
                      c_names =
                        List.filter_map
                          (fun line ->
                            Option.map
                              (fun stop -> String.sub line 0 stop)
                              (String.index_opt line '\t'))
                          mapped.manifest;
                    }
                    :: !c_units;
                  imported := mapped :: !imported;
                  Option.iter
                    (function
                      | unit_path :: fragments ->
                          prerr_endline ("fas: kept C import unit: " ^ unit_path);
                          List.iter
                            (fun path ->
                              prerr_endline ("fas: kept C fragment: " ^ path))
                            fragments
                      | [] -> ())
                    kept;
                  import rest
            in
            import c_imports
          in
          let* imported =
            C_import.merge_imports (List.rev !imported)
            |> C_import.reconcile_source program.items
          in
          let program = { Ast.items = program.items @ imported.items } in
          let* () = ast_budget (Ast.check_cumulative_asm_bytes ~limits program) in
          let* () = ast_budget (Ast.check_expanded_nodes ~limits program) in
          let* hir =
            match
              Sema.check ~limits ~c_aliases:imported.aliases
                ~c_unsupported:imported.unsupported program
            with
            | Ok hir -> Ok hir
            | Error diagnostics -> Error (add_include_chains chains diagnostics)
          in
          let* ir = Lower.lower hir in
          let* ir = apply_no_inline config ir in
          let* ir, adapters = add_static_adapters (List.rev !c_units) ir in
          let imported =
            {
              imported with
              manifest = imported.manifest @ List.map C_import.adapter_manifest adapters;
            }
          in
          let* () =
            if config.Cli.keep && c_imports <> [] then (
              let base = Filename.basename config.Cli.input in
              let name =
                try Filename.chop_extension base with Invalid_argument _ -> base
              in
              let path =
                Filename.temp_file
                  ~temp_dir:(Filename.get_temp_dir_name ())
                  (name ^ ".bindings-") ".txt"
              in
              write_file path (C_import.manifest_text imported);
              prerr_endline ("fas: kept C bindings: " ^ path));
            Ok ()
          in
          let* () = ir_budget program (Ir.check_lowered_nodes ~limits ir) in
          let* () = ir_budget program (Ir.check_stack_scratch_bytes ~limits ir) in
          let* c_objects =
            compile_c_units config cc (List.rev !c_units) adapters c_artifacts
          in
          match config.emit with
          | Cli.Ir -> (
              match Ir.render_debug_bounded ~limits ir with
              | Ok text -> emit_text config text
              | Error message -> Error [ Diag.error Span.synthetic message ])
          | Cli.Llvm ->
              let* text = render_ir ir in
              let opt, _, _ = tools () in
              let path = Filename.temp_file "fas-verify-" ".ll" in
              if config.Cli.keep then (
                prerr_endline ("fas: kept intermediate: " ^ path);
                prerr_endline
                  (Printf.sprintf "fas: tool LLVM_OPT=%s; opt passes=verify" opt));
              Fun.protect
                ~finally:(fun () -> if not config.Cli.keep then remove path)
                (fun () ->
                  write_file path text;
                  let* () = verify_llvm opt path in
                  emit_text config text)
          | Cli.Asm | Cli.Obj | Cli.Executable -> emit_tools config program ir c_objects
          ))

let run_unstaged config =
  try run_unprotected config with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]

let same_as_input config =
  canonical_path config.Cli.input = canonical_path config.Cli.output

let run config =
  if not config.Cli.output_explicit then run_unstaged config
  else if same_as_input config then
    Error [ Diag.error Span.synthetic "output path must differ from the input file" ]
  else
    let directory = Filename.dirname config.Cli.output in
    let staged =
      try Ok (Filename.temp_file ~temp_dir:directory ".fas-output-" ".tmp")
      with Sys_error message -> Error message
    in
    match staged with
    | Error message ->
        remove config.Cli.output;
        Error [ Diag.error Span.synthetic ("output I/O failed: " ^ message) ]
    | Ok staged_path -> (
        let result = run_unstaged { config with Cli.output = staged_path } in
        match result with
        | Ok output -> (
            try
              Sys.rename staged_path config.Cli.output;
              Ok output
            with Sys_error message ->
              remove staged_path;
              remove config.Cli.output;
              Error [ Diag.error Span.synthetic ("output I/O failed: " ^ message) ])
        | Error diagnostics ->
            remove staged_path;
            remove config.Cli.output;
            Error diagnostics)
