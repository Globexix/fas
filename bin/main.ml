let load_source path =
  try
    let channel = open_in_bin path in
    let text =
      Fun.protect
        ~finally:(fun () -> close_in_noerr channel)
        (fun () -> really_input_string channel (in_channel_length channel))
    in
    Some (Source.create ~file:path ~text)
  with Sys_error _ -> None

let diagnostic_source _config = function
  | diagnostic :: _ -> load_source diagnostic.Diag.primary.Span.file
  | _ -> None

let run_program (config : Cli.t) args =
  match Filename.temp_file "fas-run-" "" with
  | exception Sys_error message ->
      prerr_endline ("fas: " ^ message);
      1
  | path ->
      Fun.protect
        ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
        (fun () ->
          match
            Driver.run { config with Cli.output = path; output_explicit = true }
          with
          | Error diagnostics ->
              let source = diagnostic_source config diagnostics in
              prerr_endline (Diag.render_all ~source diagnostics);
              1
          | Ok _ -> (
              let name =
                Option.value ~default:config.input
                  (Filename.chop_suffix_opt ~suffix:".fas" config.input)
              in
              match Process.run_program path (Array.of_list (name :: args)) with
              | Ok status -> status
              | Error message ->
                  prerr_endline ("fas: cannot run " ^ path ^ ": " ^ message);
                  1))

let () =
  match Cli.parse Sys.argv with
  | Ok Cli.Help -> print_endline Cli.usage
  | Ok Cli.Version -> List.iter print_endline (Driver.version_lines ())
  | Ok (Cli.Run config) -> (
      match Driver.run config with
      | Ok output -> print_string output
      | Error diagnostics ->
          let source = diagnostic_source config diagnostics in
          prerr_endline (Diag.render_all ~source diagnostics);
          exit 1)
  | Ok (Cli.Run_program (config, args)) -> exit (run_program config args)
  | Error message ->
      prerr_endline message;
      exit 2
