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

let diagnostic_source config = function
  | diagnostic :: _ when List.mem diagnostic.Diag.primary.Span.file config.Cli.inputs ->
      load_source diagnostic.primary.file
  | _ -> None

let () =
  match Cli.parse Sys.argv with
  | Ok config -> (
      match Driver.run config with
      | Ok output -> print_string output
      | Error diagnostics ->
          let source = diagnostic_source config diagnostics in
          prerr_endline (Diag.render_all ~source diagnostics);
          exit 1)
  | Error message ->
      prerr_endline message;
      exit 2
