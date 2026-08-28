let is_space = function ' ' | '\t' | '\r' -> true | _ -> false
let is_digit c = c >= '0' && c <= '9'
let is_hex c = is_digit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
let is_ident_start c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'
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
    if offset >= n then
      let sp = Source.span source ~start_offset:n ~end_offset:n in
      Ok (List.rev ({ Token.kind = Token.Eof; span = sp } :: tokens))
    else
      let c = text.[offset] in
      if is_space c then loop (offset + 1) count tokens
      else if c = '/' && offset + 1 < n && text.[offset + 1] = '/' then
        let rec skip i = if i < n && text.[i] <> '\n' then skip (i + 1) else i in
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
      else if count >= limits.Limits.max_tokens then
        Error [ diagnostic offset "token limit exceeded" ]
      else if c = '\n' then
        let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 1) in
        loop (offset + 1) (count + 1)
          ({ Token.kind = Token.Newline; span = sp } :: tokens)
      else if
        is_ident_start c && not (c = 'c' && offset + 1 < n && text.[offset + 1] = '"')
      then
        let rec ident_end i =
          if i < n && is_ident_continue text.[i] then ident_end (i + 1) else i
        in
        let stop = ident_end (offset + 1) in
        let value = String.sub text offset (stop - offset) in
        let sp = Source.span source ~start_offset:offset ~end_offset:stop in
        loop stop (count + 1) ({ Token.kind = keyword value; span = sp } :: tokens)
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
            String.length clean >= 2
            && (String.sub clean 0 2 = "0x" || String.sub clean 0 2 = "0X")
          then (16, String.sub clean 2 (String.length clean - 2))
          else if
            String.length clean >= 2
            && (String.sub clean 0 2 = "0b" || String.sub clean 0 2 = "0B")
          then (2, String.sub clean 2 (String.length clean - 2))
          else if
            String.length clean >= 2
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
        if digits = "" then Error [ diagnostic offset "invalid integer literal" ]
        else if not (String.for_all valid_digit digits) then
          Error
            [
              diagnostic offset
                (Printf.sprintf "invalid digit in integer literal %S" raw);
            ]
        else
          let sp = Source.span source ~start_offset:offset ~end_offset:stop in
          loop stop (count + 1) ({ Token.kind = Token.Int clean; span = sp } :: tokens)
      else if c = '"' || (c = 'c' && offset + 1 < n && text.[offset + 1] = '"') then
        let string_start = if c = 'c' then offset + 1 else offset in
        let rec string_end i buffer =
          if i >= n then Error [ diagnostic offset "unterminated string literal" ]
          else
            match text.[i] with
            | '"' -> Ok (i + 1, Buffer.contents buffer)
            | '\\' when i + 1 >= n -> Error [ diagnostic i "unterminated escape" ]
            | '\\' -> (
                let value =
                  match text.[i + 1] with
                  | 'n' -> Some '\n'
                  | 't' -> Some '\t'
                  | 'r' -> Some '\r'
                  | '\\' -> Some '\\'
                  | '"' -> Some '"'
                  | '0' -> Some '\000'
                  | _ -> None
                in
                match value with
                | None -> Error [ diagnostic i "unknown string escape" ]
                | Some v ->
                    Buffer.add_char buffer v;
                    string_end (i + 2) buffer)
            | value ->
                Buffer.add_char buffer value;
                string_end (i + 1) buffer
        in
        match string_end (string_start + 1) (Buffer.create 16) with
        | Error e -> Error e
        | Ok (stop, value) ->
            let sp = Source.span source ~start_offset:offset ~end_offset:stop in
            loop stop (count + 1)
              ({
                 Token.kind =
                   (if c = 'c' then Token.CString value else Token.String value);
                 span = sp;
               }
              :: tokens)
      else
        let one kind =
          let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 1) in
          loop (offset + 1) (count + 1) ({ Token.kind; span = sp } :: tokens)
        in
        let two next kind1 kind2 =
          if offset + 1 < n && text.[offset + 1] = next then
            let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 2) in
            loop (offset + 2) (count + 1) ({ Token.kind = kind2; span = sp } :: tokens)
          else one kind1
        in
        match c with
        | '(' -> one Lparen
        | ')' -> one Rparen
        | '{' -> one Lbrace
        | '}' -> one Rbrace
        | '[' -> one Lbracket
        | ']' -> one Rbracket
        | ',' -> one Comma
        | ';' -> one Semi
        | '@' -> one At
        | '?' -> one Question
        | ':' -> one Colon
        | '~' -> one Tilde
        | '.' when offset + 2 < n && text.[offset + 1] = '.' && text.[offset + 2] = '.'
          ->
            let sp = Source.span source ~start_offset:offset ~end_offset:(offset + 3) in
            loop (offset + 3) (count + 1)
              ({ Token.kind = Ellipsis; span = sp } :: tokens)
        | '.' -> one Dot
        | '+' -> two '=' Plus Plus_eq
        | '-' -> two '=' Minus Minus_eq
        | '*' -> two '=' Star Star_eq
        | '/' -> two '=' Slash Slash_eq
        | '%' -> two '=' Percent Percent_eq
        | '=' -> two '=' Assign Eqeq
        | '!' -> two '=' Not Neq
        | '<' -> two '=' Lt Le
        | '>' -> two '=' Gt Ge
        | '&' ->
            if offset + 1 < n && text.[offset + 1] = '&' then two '&' Amp Andand
            else two '=' Amp Amp_eq
        | '|' ->
            if offset + 1 < n && text.[offset + 1] = '|' then two '|' Pipe Oror
            else two '=' Pipe Pipe_eq
        | '^' -> two '=' Caret Caret_eq
        | _ -> Error [ diagnostic offset (Printf.sprintf "unexpected character %C" c) ]
  in
  loop 0 0 []
