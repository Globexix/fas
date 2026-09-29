type failure = {
  argv : string array;
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let run_to_file argv output_path =
  if Array.length argv = 0 then
    Error
      {
        argv;
        status = Unix.WEXITED 127;
        stdout = "";
        stderr = "cannot run an empty argv";
      }
  else
    let err_path = Filename.temp_file "fas-err-" ".tmp" in
    Fun.protect
      ~finally:(fun () -> try Sys.remove err_path with Sys_error _ -> ())
      (fun () ->
        try
          let out_fd =
            Unix.openfile output_path
              [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
              0o600
          in
          let err_fd =
            Unix.openfile err_path [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
          in
          let pid =
            try Unix.create_process argv.(0) argv Unix.stdin out_fd err_fd
            with exn ->
              Unix.close out_fd;
              Unix.close err_fd;
              raise exn
          in
          Unix.close out_fd;
          Unix.close err_fd;
          let _, status = Unix.waitpid [] pid in
          let stderr = read_file err_path in
          match status with
          | Unix.WEXITED 0 -> Ok stderr
          | _ -> Error { argv; status; stdout = ""; stderr }
        with
        | Unix.Unix_error (code, operation, argument) ->
            Error
              {
                argv;
                status = Unix.WEXITED 127;
                stdout = "";
                stderr =
                  Printf.sprintf "%s(%s): %s" operation argument
                    (Unix.error_message code);
              }
        | Sys_error message ->
            Error { argv; status = Unix.WEXITED 127; stdout = ""; stderr = message })

let run argv =
  let path = Filename.temp_file "fas-out-" ".tmp" in
  Fun.protect
    ~finally:(fun () -> try Sys.remove path with Sys_error _ -> ())
    (fun () ->
      match run_to_file argv path with
      | Ok stderr -> Ok (read_file path, stderr)
      | Error failure -> Error { failure with stdout = read_file path })
