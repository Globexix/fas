let read_file path =
  try
    let channel = open_in_bin path in
    Fun.protect
      ~finally:(fun () -> close_in_noerr channel)
      (fun () ->
        let length = in_channel_length channel in
        Ok (really_input_string channel length))
  with Sys_error message -> Error message

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

let tool name default =
  match Sys.getenv_opt name with Some value when value <> "" -> value | _ -> default

let tool_error label failure =
  Diag.error Span.synthetic
    (Printf.sprintf "%s failed: %s" label failure.Process.stderr)

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

let run_llc config llc ~filetype ~input ~output =
  run_tool llc
    [|
      llc; llc_opt config.Cli.optimization; "-filetype=" ^ filetype; input; "-o"; output;
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

let emit_tools_unprotected config program ir =
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
      let opt = tool "FAS_OPT" "opt-22" in
      let llc = tool "FAS_LLC" "llc-22" in
      let cc = tool "FAS_CC" "clang-22" in
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
      match config.emit with
      | Cli.Asm ->
          let* assembly = build_assembly config ir llc opt_path asm_path in
          write_file config.output assembly;
          Ok ""
      | Cli.Obj when Ir.raw_assembly ir = "" ->
          let* () =
            run_llc config llc ~filetype:"obj" ~input:opt_path ~output:config.output
          in
          Ok ""
      | Cli.Obj ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let* () = run_tool cc [| cc; "-c"; asm_path; "-o"; config.output |] in
          Ok ""
      | Cli.Executable ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let* () = run_tool cc [| cc; asm_path; "-o"; config.output; "-no-pie" |] in
          Ok ""
      | Cli.Ast | Cli.Ir | Cli.Llvm ->
          invalid_arg "Driver.emit_tools: non-tool emission")

let emit_tools config program ir =
  try emit_tools_unprotected config program ir with
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

let run config =
  let rec parse_files acc = function
    | [] -> Ok (List.rev acc)
    | path :: rest -> (
        match read_file path with
        | Error message ->
            Error
              [
                Diag.error Span.synthetic
                  (Printf.sprintf "cannot read %s: %s" path message);
              ]
        | Ok text -> (
            let source = Source.create ~file:path ~text in
            match Parser.parse source with
            | Error diagnostics -> Error diagnostics
            | Ok program -> parse_files (program :: acc) rest))
  in
  match parse_files [] config.Cli.inputs with
  | Error diagnostics -> Error diagnostics
  | Ok programs -> (
      let program =
        {
          Ast.items = List.concat (List.map (fun program -> program.Ast.items) programs);
        }
      in
      let* () = ast_budget (Ast.check_cumulative_asm_bytes ~limits program) in
      let* () = ast_budget (Ast.check_expanded_nodes ~limits program) in
      if config.emit = Cli.Ast then
        match config.no_inline_function with
        | Some name ->
            Error
              [
                Diag.error Span.synthetic
                  (Printf.sprintf
                     "-no-inline function `%s` requires an emitted function" name);
              ]
        | None -> (
            match
              Ast.render_bounded ~budget:limits.Limits.max_rendered_ast_bytes program
            with
            | Ok text -> emit_text config text
            | Error (Ast.Render_failure (message, span)) ->
                Error [ Diag.error span message ])
      else
        let* hir = Sema.check ~limits program in
        let* ir = Lower.lower hir in
        let* ir = apply_no_inline config ir in
        let* () = ir_budget program (Ir.check_lowered_nodes ~limits ir) in
        let* () = ir_budget program (Ir.check_stack_scratch_bytes ~limits ir) in
        match config.emit with
        | Cli.Ast -> assert false
        | Cli.Ir -> (
            match Ir.render_debug_bounded ~limits ir with
            | Ok text -> emit_text config text
            | Error message -> Error [ Diag.error Span.synthetic message ])
        | Cli.Llvm ->
            let* text = render_ir ir in
            emit_text config text
        | Cli.Asm | Cli.Obj | Cli.Executable -> emit_tools config program ir)
