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

let tool name default =
  match Sys.getenv_opt name with
  | Some value when value <> "" -> value
  | _ -> default

let tool_error label failure =
  Diag.error Span.synthetic
    (Printf.sprintf "%s failed: %s" label failure.Process.stderr)

let ( let* ) result next =
  match result with
  | Ok value -> next value
  | Error diagnostics -> Error diagnostics

let run_tool label argv =
  match Process.run argv with
  | Ok _ -> Ok ()
  | Error failure -> Error [ tool_error label failure ]

let remove path = try Sys.remove path with Sys_error _ -> ()
let optimization_level level = max 0 (min 3 level)

let opt_pass level =
  match Sys.getenv_opt "FAS_OPT_PASSES" with
  | Some value when value <> "" -> value
  | _ -> Printf.sprintf "default<O%d>" (optimization_level level)

let llc_opt level = Printf.sprintf "-O%d" (optimization_level level)

let run_llc config llc ~filetype ~input ~output =
  run_tool llc
    [|
      llc;
      llc_opt config.Cli.optimization;
      "-filetype=" ^ filetype;
      input;
      "-o";
      output;
    |]

let build_assembly config ir llc opt_path asm_path =
  let* () =
    run_llc config llc ~filetype:"asm" ~input:opt_path ~output:asm_path
  in
  let* generated =
    read_file asm_path
    |> Result.map_error (fun message ->
        [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ])
  in
  let assembly = generated ^ Ir.raw_assembly ir in
  write_file asm_path assembly;
  Ok assembly

let emit_tools_unprotected config ir =
  let ll_path = Filename.temp_file "fas-module-" ".ll" in
  let opt_path = Filename.temp_file "fas-opt-" ".ll" in
  let asm_path = Filename.temp_file "fas-module-" ".s" in
  let cleanup () =
    if not config.Cli.keep then List.iter remove [ ll_path; opt_path; asm_path ]
  in
  Fun.protect ~finally:cleanup (fun () ->
      write_file ll_path (Ir.render ir);
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
          write_file (config.output ^ ".s") assembly;
          Ok assembly
      | Cli.Obj when Ir.raw_assembly ir = "" ->
          let* () =
            run_llc config llc ~filetype:"obj" ~input:opt_path
              ~output:(config.output ^ ".o")
          in
          Ok ""
      | Cli.Obj ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let* () =
            run_tool cc [| cc; "-c"; asm_path; "-o"; config.output ^ ".o" |]
          in
          Ok ""
      | Cli.Executable ->
          let* _ = build_assembly config ir llc opt_path asm_path in
          let* () =
            run_tool cc [| cc; asm_path; "-o"; config.output; "-no-pie" |]
          in
          Ok ""
      | Cli.Ast | Cli.Ir | Cli.Llvm ->
          invalid_arg "Driver.emit_tools: non-tool emission")

let emit_tools config ir =
  try emit_tools_unprotected config ir with
  | Sys_error message ->
      Error [ Diag.error Span.synthetic ("backend I/O failed: " ^ message) ]
  | Unix.Unix_error (code, operation, argument) ->
      Error
        [
          Diag.error Span.synthetic
            (Printf.sprintf "backend %s(%s) failed: %s" operation argument
               (Unix.error_message code));
        ]
