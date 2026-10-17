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

let report (config : Cli.t) diagnostics =
  match config.diagnostics with
  | Cli.Text ->
      let source = diagnostic_source config diagnostics in
      prerr_endline (Diag.render_all ~source diagnostics)
  | Cli.Json -> prerr_endline (Diag.render_json diagnostics)

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
              report config diagnostics;
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

let load_baseline (options : Cli.perf_options) =
  match options.compare with
  | None -> Ok None
  | Some path -> Result.map Option.some (Perf_report.load_baseline path)

let print_report (options : Cli.perf_options) baseline result =
  let diff =
    match baseline with
    | None -> Ok None
    | Some baseline -> Result.map Option.some (Perf_report.compare_with baseline result)
  in
  match diff with
  | Error message -> Error message
  | Ok diff ->
      if options.json then print_endline (Perf_report.json_report ?diff result)
      else List.iter prerr_endline (Perf_report.text_lines ?diff result);
      Ok ()

let native_rows (config : Cli.t) names dir =
  if not (Perf_tools.mca_available ()) then None
  else
    let exe = Filename.concat dir "native" in
    let remarks = Filename.concat dir "native-remarks.yaml" in
    let regalloc = Filename.concat dir "native-regalloc.yaml" in
    match
      Driver.run ~remarks ~llc_remarks:regalloc ~c_debug:true
        {
          config with
          Cli.debug = true;
          keep = false;
          target_cpu = Some "native";
          target = Cli.Portable;
          output = exe;
          output_explicit = true;
        }
    with
    | Error _ -> None
    | Ok _ -> (
        match Perf.located ~input:config.input names exe with
        | Ok (rows, _) ->
            Some
              {
                Perf.other = rows;
                other_notes =
                  Perf_tools.read_remarks remarks @ Perf_tools.read_remarks regalloc;
              }
        | Error _ -> None)

let perf_command (config : Cli.t) (options : Cli.perf_options) baseline args =
  match Filename.temp_dir "fas-perf-" "" with
  | exception Sys_error message ->
      prerr_endline ("fas: " ^ message);
      1
  | dir ->
      let exe = Filename.concat dir "program" in
      let remarks = Filename.concat dir "remarks.yaml" in
      let regalloc = Filename.concat dir "regalloc.yaml" in
      let data = Filename.concat dir "perf.data" in
      let llvm = ref "" in
      let build =
        Driver.run ~remarks ~llc_remarks:regalloc ~c_debug:true
          ~llvm_text:(fun text -> llvm := text)
          { config with Cli.debug = true; output = exe; output_explicit = true }
      in
      let status =
        match build with
        | Error diagnostics ->
            report config diagnostics;
            1
        | Ok _ -> (
            let names = Perf_tools.display_names !llvm in
            match Perf.located ~input:config.input names exe with
            | Error message ->
                prerr_endline message;
                1
            | Ok (rows, starts) -> (
                match Perf.select options.asm options.copy rows with
                | Error message ->
                    prerr_endline message;
                    2
                | Ok view -> (
                    let notes =
                      Perf_tools.read_remarks remarks @ Perf_tools.read_remarks regalloc
                    in
                    let program =
                      {
                        Perf.input = config.input;
                        exe;
                        names;
                        notes;
                        rows;
                        starts;
                        target = native_rows config names dir;
                      }
                    in
                    let show result =
                      match Result.bind result (print_report options baseline) with
                      | Ok () -> 0
                      | Error message ->
                          prerr_endline message;
                          1
                    in
                    if options.static then show (Perf.static_report program view)
                    else
                      let perf = Driver.tool "FAS_PERF" "perf" in
                      match Perf_tools.record ~perf ~data exe args with
                      | Error reason ->
                          ignore (show (Perf.static_report program view));
                          prerr_endline ("fas perf: no run: " ^ reason);
                          1
                      | Ok (status, text) -> (
                          match
                            show
                              (Perf.profile program view ~top:options.top ~status text)
                          with
                          | 0 -> status
                          | failed -> failed))))
      in
      (if not config.keep then
         try
           Array.iter
             (fun name -> Sys.remove (Filename.concat dir name))
             (Sys.readdir dir);
           Sys.rmdir dir
         with Sys_error _ -> ());
      status

let () =
  match Cli.parse Sys.argv with
  | Ok Cli.Help -> print_endline Cli.usage
  | Ok Cli.Version -> List.iter print_endline (Driver.version_lines ())
  | Ok (Cli.Run config) -> (
      match Driver.run config with
      | Ok output -> print_string output
      | Error diagnostics ->
          report config diagnostics;
          exit 1)
  | Ok (Cli.Check config) -> (
      match Driver.check config with
      | Ok checked ->
          Driver.release checked;
          if config.diagnostics = Cli.Json then prerr_endline (Diag.render_json []);
          exit 0
      | Error diagnostics ->
          report config diagnostics;
          exit 1)
  | Ok (Cli.Lsp options) -> exit (Lsp.run options)
  | Ok (Cli.Run_program (config, args)) -> exit (run_program config args)
  | Ok (Cli.Perf (config, options, args)) -> (
      match load_baseline options with
      | Ok baseline -> exit (perf_command config options baseline args)
      | Error message ->
          prerr_endline message;
          exit 1)
  | Error message ->
      prerr_endline message;
      exit 2
