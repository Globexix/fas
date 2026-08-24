let is_space = function ' ' | '\t' | '\r' -> true | _ -> false
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

let is_ident_start c =
  (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let is_ident_continue c = is_ident_start c || is_digit c

let keyword = function
  | "fn" -> Token.Kw_fn
  | "return" -> Kw_return
  | "if" -> Kw_if
  | "else" -> Kw_else
  | "while" -> Kw_while
  | "break" -> Kw_break
  | "continue" -> Kw_continue
  | "const" -> Kw_const
  | "struct" -> Kw_struct
  | "opaque" -> Kw_opaque
  | "extern" -> Kw_extern
  | "defer" -> Kw_defer
  | "asm" -> Kw_asm
  | "for" -> Kw_for
  | "switch" -> Kw_switch
  | "case" -> Kw_case
  | "default" -> Kw_default
  | name -> Ident name

let lex ?(limits = Limits.default) source =
  let text = Source.text source in
  let n = String.length text in
  let diagnostic offset message =
    Diag.error
      (Source.span source ~start_offset:offset ~end_offset:(min n (offset + 1)))
      message
  in
  let rec loop offset count tokens =
    if count >= limits.Limits.max_tokens then
      Error [ diagnostic offset "token limit exceeded" ]
    else if offset >= n then
      let sp = Source.span source ~start_offset:n ~end_offset:n in
      Ok (List.rev ({ Token.kind = Token.Eof; span = sp } :: tokens))
    else
      let c = text.[offset] in
      if is_space c then loop (offset + 1) count tokens
      else if c = '\n' then
        let sp =
          Source.span source ~start_offset:offset ~end_offset:(offset + 1)
        in
        loop (offset + 1) (count + 1)
          ({ Token.kind = Token.Newline; span = sp } :: tokens)
      else if c = '/' && offset + 1 < n && text.[offset + 1] = '/' then
        let rec skip i =
          if i < n && text.[i] <> '\n' then skip (i + 1) else i
        in
        loop (skip (offset + 2)) count tokens
      else if c = '/' && offset + 1 < n && text.[offset + 1] = '*' then
        let rec close i =
          if i + 1 >= n then None
          else if text.[i] = '*' && text.[i + 1] = '/' then Some (i + 2)
          else close (i + 1)
        in
        match close (offset + 2) with
        | None -> Error [ diagnostic offset "unterminated block comment" ]
        | Some next -> loop next count tokens
      else if is_ident_start c then
        let rec ident_end i =
          if i < n && is_ident_continue text.[i] then ident_end (i + 1) else i
        in
        let stop = ident_end (offset + 1) in
        let value = String.sub text offset (stop - offset) in
        let sp = Source.span source ~start_offset:offset ~end_offset:stop in
        loop stop (count + 1)
          ({ Token.kind = keyword value; span = sp } :: tokens)
      else if is_digit c then
        let rec number_end i =
          if i < n && (is_ident_continue text.[i] || text.[i] = '_') then
            number_end (i + 1)
          else i
        in
        let stop = number_end (offset + 1) in
        let raw = String.sub text offset (stop - offset) in
        let clean = String.concat "" (String.split_on_char '_' raw) in
        let radix, digits =
          if
            String.length clean > 2
            && (String.sub clean 0 2 = "0x" || String.sub clean 0 2 = "0X")
          then (16, String.sub clean 2 (String.length clean - 2))
          else if
            String.length clean > 2
            && (String.sub clean 0 2 = "0b" || String.sub clean 0 2 = "0B")
          then (2, String.sub clean 2 (String.length clean - 2))
          else if
            String.length clean > 2
            && (String.sub clean 0 2 = "0o" || String.sub clean 0 2 = "0O")
          then (8, String.sub clean 2 (String.length clean - 2))
          else (10, clean)
        in
        let valid_digit c =
          match radix with
          | 2 -> c = '0' || c = '1'
          | 8 -> c >= '0' && c <= '7'
          | 10 -> is_digit c
          | 16 -> is_hex c
          | _ -> false
        in
        if digits = "" then
          Error [ diagnostic offset "integer literal has no digits" ]
        else if not (String.for_all valid_digit digits) then
          Error
            [
              diagnostic offset
                (Printf.sprintf "invalid digit in integer literal %S" raw);
            ]
        else
          let sp = Source.span source ~start_offset:offset ~end_offset:stop in
          loop stop (count + 1)
            ({ Token.kind = Token.Int clean; span = sp } :: tokens)
      else if c = '"' then
        let rec string_end i buffer =
          if i >= n then
            Error [ diagnostic offset "unterminated string literal" ]
          else
            match text.[i] with
            | '"' -> Ok (i + 1, Buffer.contents buffer)