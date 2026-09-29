type header = { spelling : Ast.c_header; span : Span.t }

let include_line source = function
  | { spelling = Ast.C_quoted path; _ } ->
      let path =
        if Filename.is_relative path then Filename.concat (Filename.dirname source) path
        else path
      in
      Printf.sprintf "#include %S\n" path
  | { spelling = Ast.C_system path; _ } -> "#include <" ^ path ^ ">\n"

let find_text text needle start =
  let limit = String.length text - String.length needle in
  let rec find index =
    if index > limit then None
    else if String.sub text index (String.length needle) = needle then Some index
    else find (index + 1)
  in
  find start

let first_error output =
  String.split_on_char '\n' output
  |> List.find_opt (fun line -> Option.is_some (find_text line "error:" 0))

let unit_line unit_path output =
  match find_text output (unit_path ^ ":") 0 with
  | None -> None
  | Some start -> (
      let digit = start + String.length unit_path + 1 in
      let finish = try String.index_from output digit ':' with Not_found -> digit in
      try Some (int_of_string (String.sub output digit (finish - digit)))
      with Failure _ -> None)

let error_span headers line =
  Option.bind line (fun line -> List.nth_opt headers (line - 1)) |> function
  | None -> (List.hd headers).span
  | Some header -> header.span

let import ~cc ~debug ~keep source headers =
  let unit_path = Filename.temp_file "fas-c-import-" ".c" in
  let json_path = Filename.temp_file "fas-c-import-" ".json" in
  let cleanup () =
    let remove path = try Sys.remove path with Sys_error _ -> () in
    if not keep then remove unit_path;
    remove json_path
  in
  Fun.protect ~finally:cleanup (fun () ->
      let channel = open_out_bin unit_path in
      Fun.protect
        ~finally:(fun () -> close_out_noerr channel)
        (fun () ->
          List.iter
            (fun header -> output_string channel (include_line source header))
            headers);
      let argv =
        [|
          cc;
          "-x";
          "c";
          "-fsyntax-only";
          "-Xclang";
          "-ast-dump=json";
          "-Xclang";
          "-skip-function-bodies";
          "--target=x86_64-unknown-linux-gnu";
          unit_path;
        |]
      in
      if debug || keep then
        prerr_endline
          ("fas: Clang import command: " ^ String.concat " " (Array.to_list argv));
      match Process.run_to_file argv json_path with
      | Ok _ -> (
          try
            let channel = open_in_bin json_path in
            let declarations =
              Fun.protect
                ~finally:(fun () -> close_in_noerr channel)
                (fun () -> C_import_json.declarations channel)
            in
            Ok (declarations, if keep then Some unit_path else None)
          with Failure message ->
            Error
              [
                Diag.error (List.hd headers).span
                  ("internal error: C import JSON reader: " ^ message);
              ])
      | Error failure ->
          let message =
            Option.value ~default:(String.trim failure.stderr)
              (first_error failure.stderr)
          in
          let span = error_span headers (unit_line unit_path failure.stderr) in
          Error
            [
              Diag.error span
                (if message = "" then "Clang C header import failed"
                 else "Clang C header import failed: " ^ message);
            ])
