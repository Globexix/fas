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

let run argv =
  if Array.length argv = 0 then
    Error
      {
        argv;
        status = Unix.WEXITED 127;
        stdout = "";
        stderr = "cannot run an empty argv";
      }
  else
    let out_path = Filename.temp_file "fas-out-" ".tmp" in
    let err_path = Filename.temp_file "fas-err-" ".tmp" in
    let cleanup () =
      List.iter
        (fun path -> try Sys.remove path with Sys_error _ -> ())
        [ out_path; err_path ]
    in
    Fun.protect ~finally:cleanup (fun () ->
        try
          let out_fd =
            Unix.openfile out_path
              [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
              0o600
          in
          let err_fd =
            try
              Unix.openfile err_path
                [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ]
                0o600
            with exn ->
              Unix.close out_fd;
              raise exn
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
          let stdout = read_file out_path in
          let stderr = read_file err_path in
          match status with
          | Unix.WEXITED 0 -> Ok (stdout, stderr)
          | _ -> Error { argv; status; stdout; stderr }
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
            Error
              { argv; status = Unix.WEXITED 127; stdout = ""; stderr = message })
