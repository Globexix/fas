type raw_body = { name : string; text : string }

let ident_char c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_'

let word_at text i word =
  let n = String.length text and m = String.length word in
  i + m <= n
  && String.sub text i m = word
  && (i + m = n || not (ident_char text.[i + m]))
  && (i = 0 || not (ident_char text.[i - 1]))

let skip_space_comments text i =
  let n = String.length text in
  let rec loop j =
    if j >= n then j
    else if text.[j] = ' ' || text.[j] = '\t' || text.[j] = '\r' || text.[j] = '\n' then
      loop (j + 1)
    else if j + 1 < n && text.[j] = '/' && text.[j + 1] = '/' then
      let rec line k = if k < n && text.[k] <> '\n' then line (k + 1) else k in
      loop (line (j + 2))
    else if j + 1 < n && text.[j] = '/' && text.[j + 1] = '*' then
      let rec block k =
        if k + 1 < n && not (text.[k] = '*' && text.[k + 1] = '/') then block (k + 1)
        else min n (k + 2)
      in
      loop (block (j + 2))
    else j
  in
  loop i

let blank_range buffer text start stop =
  for i = start to stop - 1 do
    if text.[i] <> '\n' then Bytes.set buffer i ' '
  done

let extract_asm source limits =
  let text = Source.text source and n = Source.length source in
  let buffer = Bytes.of_string text in
  let bodies = ref [] in
  let rec find_body_open i quote line_comment block_comment =
    if i >= n then None
    else if line_comment then
      find_body_open (i + 1) quote (text.[i] <> '\n') block_comment
    else if block_comment then
      if i + 1 < n && text.[i] = '*' && text.[i + 1] = '/' then
        find_body_open (i + 2) quote false false
      else find_body_open (i + 1) quote false true
    else
      match quote with
      | Some _ when text.[i] = '\\' -> find_body_open (i + 2) quote false false
      | Some q when text.[i] = q -> find_body_open (i + 1) None false false
      | Some _ -> find_body_open (i + 1) quote false false
      | None when i + 1 < n && text.[i] = '/' && text.[i + 1] = '/' ->
          find_body_open (i + 2) None true false
      | None when i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' ->
          find_body_open (i + 2) None false true
      | None when text.[i] = '#' -> find_body_open (i + 1) None true false
      | None when text.[i] = '"' || text.[i] = '\'' ->
          find_body_open (i + 1) (Some text.[i]) false false
      | None when text.[i] = '{' -> Some i
      | None -> find_body_open (i + 1) None false false
  in
  let matching_body open_pos =
    let rec scan i depth quote line_comment block_comment =
      if i >= n then None
      else if line_comment then
        scan (i + 1) depth quote (text.[i] <> '\n') block_comment
      else if block_comment then
        if i + 1 < n && text.[i] = '*' && text.[i + 1] = '/' then
          scan (i + 2) depth quote false false
        else scan (i + 1) depth quote false true
      else
        match quote with
        | Some _ when text.[i] = '\\' -> scan (i + 2) depth quote false false
        | Some q when text.[i] = q -> scan (i + 1) depth None false false
        | Some _ -> scan (i + 1) depth quote false false
        | None when i + 1 < n && text.[i] = '/' && text.[i + 1] = '/' ->
            scan (i + 2) depth None true false
        | None when i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' ->
            scan (i + 2) depth None false true
        | None when text.[i] = '#' -> scan (i + 1) depth None true false
        | None when text.[i] = '"' || text.[i] = '\'' ->
            scan (i + 1) depth (Some text.[i]) false false
        | None when text.[i] = '{' -> scan (i + 1) (depth + 1) None false false
        | None when text.[i] = '}' ->
            if depth = 1 then Some i else scan (i + 1) (depth - 1) None false false
        | None -> scan (i + 1) depth None false false
    in
    scan (open_pos + 1) 1 None false false
  in
  let rec scan i =
    if i >= n then Ok (Bytes.to_string buffer, List.rev !bodies)
    else if text.[i] = '"' || text.[i] = '\'' then
      let quote = text.[i] in
      let rec skip j =
        if j >= n then n
        else if text.[j] = '\\' then skip (j + 2)
        else if text.[j] = quote then j + 1
        else skip (j + 1)
      in
      scan (skip (i + 1))
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '/' then
      let rec skip j = if j < n && text.[j] <> '\n' then skip (j + 1) else j in
      scan (skip (i + 2))
    else if i + 1 < n && text.[i] = '/' && text.[i + 1] = '*' then
      let rec skip j =
        if j + 1 < n && not (text.[j] = '*' && text.[j + 1] = '/') then skip (j + 1)
        else min n (j + 2)
      in
      scan (skip (i + 2))
    else if word_at text i "asm" then
      let fn_pos = skip_space_comments text (i + 3) in
      if word_at text fn_pos "fn" then
        let name_pos = skip_space_comments text (fn_pos + 2) in
        let name_end =
          let rec f j = if j < n && ident_char text.[j] then f (j + 1) else j in
          f name_pos
        in
        let name =
          if name_end = name_pos then "<anonymous>"
          else String.sub text name_pos (name_end - name_pos)
        in
        let open_pos =
          match find_body_open name_end None false false with None -> n | Some p -> p
        in
        if open_pos = n then
          Error
            [
              Diag.error
                (Source.span source ~start_offset:i ~end_offset:(min n (i + 3)))
                ("asm fn " ^ name ^ " has no body");
            ]
        else
          match matching_body open_pos with
          | None ->
              Error
                [
                  Diag.error
                    (Source.span source ~start_offset:open_pos
                       ~end_offset:(min n (open_pos + 1)))
                    ("unterminated asm body for " ^ name);
                ]
          | Some close_pos ->
              let body = String.sub text (open_pos + 1) (close_pos - open_pos - 1) in
              if String.length body > limits.Limits.max_asm_bytes then
                Error
                  [
                    Diag.error
                      (Source.span source ~start_offset:open_pos ~end_offset:close_pos)
                      "raw asm body exceeds the configured limit";
                  ]
              else begin
                bodies := { name; text = body } :: !bodies;
                blank_range buffer text (open_pos + 1) close_pos;
                scan (close_pos + 1)
              end
      else scan (i + 1)
    else scan (i + 1)
  in
  scan 0
