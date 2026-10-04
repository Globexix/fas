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

let extract_containers source =
  let text = Source.text source and n = Source.length source in
  let buffer = Bytes.of_string text in
  let c_containers = ref [] in
  let at_line_start i =
    i = 0
    ||
    let start =
      1 + Option.value ~default:(-1) (String.rindex_from_opt text (i - 1) '\n')
    in
    String.for_all
      (function ' ' | '\t' | '\r' -> true | _ -> false)
      (String.sub text start (i - start))
  in
  let line_end i = Option.value ~default:n (String.index_from_opt text i '\n') in
  let rec skip_while stop predicate i =
    if i < stop && predicate text.[i] then skip_while stop predicate (i + 1) else i
  in
  let container_prefix i =
    let end_pos = line_end i in
    let stop =
      if end_pos > i && text.[end_pos - 1] = '\r' then end_pos - 1 else end_pos
    in
    let j = skip_while stop (fun c -> c = ' ' || c = '\t') (i + 3) in
    let width =
      if j + 3 <= stop && String.sub text j 3 = "\"C\"" then 3
      else if j + 5 <= stop && String.sub text j 5 = "\"asm\"" then 5
      else 0
    in
    if width = 0 then None
    else
      let j = skip_while stop (fun c -> c = ' ' || c = '\t') (j + width) in
      if j + 2 > stop || String.sub text j 2 <> "<<" then None
      else
        let tag_start = j + 2 in
        let tag_stop = skip_while stop (fun c -> c <> ' ' && c <> '\t') tag_start in
        let tag = String.sub text tag_start (tag_stop - tag_start) in
        let valid =
          tag <> ""
          && ident_char tag.[0]
          && (not (tag.[0] >= '0' && tag.[0] <= '9'))
          && String.for_all ident_char tag
        in
        if not valid then Some (Error "C container tag must be an ASCII identifier")
        else if tag_stop <> stop then Some (Error "trailing text after C container tag")
        else Some (Ok tag)
  in
  let find_terminator tag start =
    let rec scan line_start =
      let stop = line_end line_start in
      let line = String.sub text line_start (stop - line_start) in
      if line = tag || line = tag ^ "\r" then Some (line_start, min n (stop + 1))
      else if stop = n then None
      else scan (stop + 1)
    in
    scan start
  in
  let container_error i message =
    Error
      [ Diag.error (Source.span source ~start_offset:i ~end_offset:(i + 3)) message ]
  in
  let extract_container i tag =
    let payload_start = min n (line_end i + 1) in
    match find_terminator tag payload_start with
    | None -> container_error i ("C container is missing terminator `" ^ tag ^ "`")
    | Some (terminator_start, after_terminator) ->
        c_containers :=
          ( i,
            Ast.
              {
                tag;
                text = String.sub text payload_start (terminator_start - payload_start);
              } )
          :: !c_containers;
        blank_range buffer text payload_start after_terminator;
        Ok after_terminator
  in
  let rec scan i =
    if i >= n then Ok (Bytes.to_string buffer, List.rev !c_containers)
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
    else if text.[i] = 'u' && word_at text i "use" && at_line_start i then
      match container_prefix i with
      | None -> scan (i + 1)
      | Some (Error message) -> container_error i message
      | Some (Ok tag) -> Result.bind (extract_container i tag) scan
    else if word_at text i "asm" && word_at text (skip_space_comments text (i + 3)) "fn"
    then
      container_error i
        "asm fn was removed in v0.3; use a use \"asm\" unit and an extern \"C\" \
         declaration"
    else scan (i + 1)
  in
  scan 0

module P = struct
  type t = {
    tokens : Token.t array;
    mutable pos : int;
    c_containers : (int * Ast.c_fragment) list;
    limits : Limits.t;
    mutable depth : int;
    mutable nesting : int;
    mutable block_expression_depth : int option;
    mutable delimited_depth : int;
  }

  let skip_delimited_newlines p =
    while
      p.delimited_depth > 0
      && p.pos < Array.length p.tokens - 1
      && p.tokens.(p.pos).Token.kind = Token.Newline
    do
      p.pos <- p.pos + 1
    done

  let peek p =
    skip_delimited_newlines p;
    p.tokens.(min p.pos (Array.length p.tokens - 1))

  let peek_n p n =
    skip_delimited_newlines p;
    let rec find i remaining =
      if i >= Array.length p.tokens - 1 then p.tokens.(Array.length p.tokens - 1)
      else if p.delimited_depth > 0 && p.tokens.(i).Token.kind = Token.Newline then
        find (i + 1) remaining
      else if remaining = 0 then p.tokens.(i)
      else find (i + 1) (remaining - 1)
    in
    find p.pos n

  let bump p =
    let t = peek p in
    if p.pos < Array.length p.tokens - 1 then p.pos <- p.pos + 1;
    t

  let at p k = (peek p).Token.kind = k

  let eat p k =
    if at p k then (
      ignore (bump p);
      true)
    else false

  let expected p kind =
    if eat p kind then Ok ()
    else
      Error
        [
          Diag.error (peek p).span
            ("expected " ^ Token.show kind ^ ", found " ^ Token.show (peek p).kind);
        ]

  let ident p =
    match (bump p).kind with
    | Token.Ident s -> Ok s
    | t ->
        Error
          [ Diag.error (peek p).span ("expected identifier, found " ^ Token.show t) ]

  let field_ident p =
    let token = bump p in
    match token.kind with
    | Token.Ident name -> Ok (name, token.span)
    | ( Token.Kw_fn | Token.Kw_return | Token.Kw_if | Token.Kw_else | Token.Kw_while
      | Token.Kw_break | Token.Kw_continue | Token.Kw_const | Token.Kw_var
      | Token.Kw_struct | Token.Kw_opaque | Token.Kw_extern | Token.Kw_defer
      | Token.Kw_use | Token.Kw_for | Token.Kw_switch | Token.Kw_case | Token.Kw_default
        ) as kind ->
        let shown = Token.show kind in
        Ok (String.sub shown 1 (String.length shown - 2), token.span)
    | kind ->
        Error
          [ Diag.error token.span ("expected identifier, found " ^ Token.show kind) ]

  let skip_newlines p =
    while at p Token.Newline do
      ignore (bump p)
    done

  let delimited p parse =
    let previous = p.delimited_depth in
    p.delimited_depth <- previous + 1;
    let result = parse () in
    p.delimited_depth <- previous;
    result

  let end_stmt p =
    if eat p Token.Semi then (
      skip_newlines p;
      Ok ())
    else if at p Token.Newline then (
      skip_newlines p;
      Ok ())
    else if at p Token.Rbrace || at p Token.Eof then Ok ()
    else Error [ Diag.error (peek p).span "expected end of statement (newline or `;`)" ]

  let string p =
    match (bump p).kind with
    | Token.String s -> Ok s
    | t ->
        Error
          [
            Diag.error (peek p).span ("expected string literal, found " ^ Token.show t);
          ]

  let integer p =
    match (bump p).kind with
    | Token.Int s -> Ok s
    | t ->
        Error [ Diag.error (peek p).span ("expected integer, found " ^ Token.show t) ]

  let aggregate_length p =
    match (bump p).kind with
    | Token.Int s | Token.Ident s -> Ok s
    | t ->
        Error
          [
            Diag.error (peek p).span
              ("expected integer or const parameter, found " ^ Token.show t);
          ]

  let int_value p =
    match integer p with
    | Error e -> Error e
    | Ok s -> (
        try Ok (int_of_string s)
        with Failure _ -> Error [ Diag.error (peek p).span "integer is out of range" ])

  let bind r f = match r with Error e -> Error e | Ok x -> f x
  let ( let* ) = bind
  let span p = (peek p).Token.span

  let within_nesting p s message parse =
    p.nesting <- p.nesting + 1;
    if p.nesting + p.depth > p.limits.Limits.max_nesting then (
      p.nesting <- p.nesting - 1;
      Error [ Diag.error s message ])
    else
      let result = parse () in
      p.nesting <- p.nesting - 1;
      result

  let rec ty p =
    within_nesting p (span p) "type nesting exceeds the configured limit" (fun () ->
        ty_inner p)

  and ty_inner p =
    match (peek p).kind with
    | Token.Ident "bool" ->
        ignore (bump p);
        Ok Ast.Bool
    | Token.Ident "void" ->
        ignore (bump p);
        Ok Ast.Void
    | Token.Ident "ptr" ->
        let s = span p in
        ignore (bump p);
        Error
          [
            Diag.error s "typed pointers are no longer supported; use addr or handle[T]";
          ]
    | Token.Ident name when Names.type_constructor name = Some Names.Address ->
        ignore (bump p);
        Ok Ast.Addr
    | Token.Ident name when Names.type_constructor name = Some Names.Handle ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Handle t))
    | Token.Ident name when Names.type_constructor name = Some Names.Array ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* n = aggregate_length p in
            let* () = expected p Token.Comma in
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Array (n, t)))
    | Token.Ident name when Names.type_constructor name = Some Names.Vector ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        delimited p (fun () ->
            let* n = aggregate_length p in
            let* () = expected p Token.Comma in
            let* t = ty p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Vec (n, t)))
    | Token.Ident s when List.mem s Names.scalar_type_names -> (
        ignore (bump p);
        match s with
        | "u8" -> Ok (Ast.Int Ast.U8)
        | "u16" -> Ok (Ast.Int Ast.U16)
        | "u32" -> Ok (Ast.Int Ast.U32)
        | "u64" -> Ok (Ast.Int Ast.U64)
        | "i8" -> Ok (Ast.Int Ast.I8)
        | "i16" -> Ok (Ast.Int Ast.I16)
        | "i32" -> Ok (Ast.Int Ast.I32)
        | "i64" -> Ok (Ast.Int Ast.I64)
        | "usize" -> Ok (Ast.Int Ast.Usize)
        | "isize" -> Ok (Ast.Int Ast.Isize)
        | _ -> assert false)
    | Token.Ident s ->
        ignore (bump p);
        if at p Token.Lbracket then
          let span = span p in
          let* args = generic_args p in
          Ok (Ast.Applied_type (s, args, span))
        else Ok (Ast.Named_type s)
    | t -> Error [ Diag.error (span p) ("expected a type, found " ^ Token.show t) ]

  and select_payloads p =
    delimited p (fun () ->
        let rec go acc =
          let* arg = select_payload p in
          if eat p Token.Comma then go (arg :: acc)
          else
            let* () = expected p Token.Rbracket in
            Ok (List.rev (arg :: acc))
        in
        go [])

  and select_payload p = generic_arg p

  and generic_args p =
    let* () = expected p Token.Lbracket in
    delimited p (fun () ->
        let rec go acc =
          let* arg = generic_arg p in
          if eat p Token.Comma then go (arg :: acc)
          else
            let* () = expected p Token.Rbracket in
            Ok (List.rev (arg :: acc))
        in
        go [])

  and generic_arg p =
    match ((peek p).kind, (peek_n p 1).kind) with
    | Token.Ident ("true" | "false"), _ ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident ("sizeof" | "alignof" | "offsetof"), Token.Lbracket ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident _, Token.Lbracket when generic_application_is_call p ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident name, (Token.Comma | Token.Rbracket)
      when not (Names.parser_type_name name) ->
        let s = span p in
        ignore (bump p);
        Ok (Ast.Name_arg (name, s))
    | Token.Ident name, _ when Names.parser_type_name name ->
        let* t = ty p in
        Ok (Ast.Type_arg t)
    | Token.Ident _, Token.Lbracket ->
        let previous = p.pos in
        let* t = ty p in
        if at p Token.Comma || at p Token.Rbracket then Ok (Ast.Type_or_index t)
        else (
          p.pos <- previous;
          let* e = expr p in
          Ok (Ast.Const_arg e))
    | _ ->
        let* e = expr p in
        Ok (Ast.Const_arg e)

  and generic_application_is_call p =
    let rec scan offset depth =
      match (peek_n p offset).kind with
      | Token.Lbracket -> scan (offset + 1) (depth + 1)
      | Token.Rbracket when depth = 1 -> (peek_n p (offset + 1)).kind = Token.Lparen
      | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
      | Token.Newline when depth > 0 -> scan (offset + 1) depth
      | Token.Eof | Token.Newline -> false
      | _ -> scan (offset + 1) depth
    in
    scan 1 0

  and starts_type p =
    match ((peek p).kind, (peek_n p 1).kind) with
    | Token.Ident _, Token.Ident _ -> true
    | _ -> false

  and starts_struct_literal p =
    let rec scan offset brackets =
      match (peek_n p offset).kind with
      | Token.Lbracket -> scan (offset + 1) (brackets + 1)
      | Token.Rbracket when brackets > 0 -> scan (offset + 1) (brackets - 1)
      | Token.Rparen when brackets = 0 -> (peek_n p (offset + 1)).kind = Token.Lbrace
      | Token.Newline -> scan (offset + 1) brackets
      | Token.Eof -> false
      | _ -> scan (offset + 1) brackets
    in
    p.block_expression_depth <> Some p.depth
    && match (peek_n p 1).kind with Token.Ident _ -> scan 1 0 | _ -> false

  and compound_op = function
    | Token.Plus_eq -> Some Ast.Add
    | Token.Minus_eq -> Some Ast.Sub
    | Token.Star_eq -> Some Ast.Mul
    | Token.Slash_eq -> Some Ast.Div
    | Token.Percent_eq -> Some Ast.Rem
    | Token.Amp_eq -> Some Ast.Bit_and
    | Token.Pipe_eq -> Some Ast.Bit_or
    | Token.Caret_eq -> Some Ast.Bit_xor
    | Token.Ltlt_eq -> Some Ast.Shl
    | Token.Gtgt_eq -> Some Ast.Shr
    | _ -> None

  and finish_statement p consume_end = if consume_end then end_stmt p else Ok ()

  and c_header p use_span =
    match (peek p).kind with
    | Token.String header ->
        ignore (bump p);
        Ok (Ast.C_quoted header)
    | Token.Ltlt -> (
        ignore (bump p);
        let* tag = ident p in
        match List.assoc_opt use_span.Span.start_offset p.c_containers with
        | Some fragment when fragment.Ast.tag = tag -> Ok (Ast.C_fragment fragment)
        | _ -> Error [ Diag.error use_span "C container extraction failed" ])
    | Token.Newline | Token.Eof ->
        Error [ Diag.error use_span "use \"C\" is not implemented until v0.2" ]
    | Token.Lt ->
        ignore (bump p);
        let rec path parts =
          match (bump p).kind with
          | Token.Gt ->
              if parts = [] then
                Error [ Diag.error use_span "C header name cannot be empty" ]
              else Ok (Ast.C_system (String.concat "" (List.rev parts)))
          | Token.Dot -> path ("." :: parts)
          | Token.Slash -> path ("/" :: parts)
          | Token.Minus -> path ("-" :: parts)
          | Token.Plus -> path ("+" :: parts)
          | Token.Newline | Token.Eof ->
              Error [ Diag.error use_span "expected `>` in C header name" ]
          | kind -> (
              match Token.c_header_component kind with
              | Some component -> path (component :: parts)
              | None -> Error [ Diag.error use_span "expected `>` in C header name" ])
        in
        path []
    | _ -> Error [ Diag.error use_span "expected a quoted or angle-bracket C header" ]

  and items p =
    skip_newlines p;
    if at p Token.Eof then Ok []
    else
      let* xs = item p in
      let* ys = items p in
      Ok (xs @ ys)

  and item p =
    match (peek p).kind with
    | Token.Kw_use ->
        let s = span p in
        ignore (bump p);
        let* path = string p in
        if path = "C" || path = "asm" then
          let header_start = span p in
          let* header = c_header p s in
          let* () = end_stmt p in
          let span =
            match header with
            | Ast.C_quoted _ | Ast.C_system _ -> header_start
            | Ast.C_fragment _ -> s
          in
          Ok [ Ast.Use { path; c_header = Some header; span } ]
        else
          let* () = end_stmt p in
          Ok [ Ast.Use { path; c_header = None; span = s } ]
    | Token.Kw_const ->
        let* x = const_item p in
        Ok [ x ]
    | Token.Kw_var ->
        let* x = global_item p Ast.Internal_global in
        Ok [ x ]
    | Token.Kw_struct ->
        let* x = struct_item p in
        Ok [ x ]
    | Token.Kw_opaque ->
        let* x = opaque_item p in
        Ok [ x ]
    | Token.Kw_extern -> extern_block p
    | Token.Kw_fn ->
        let* x = fn_item p false in
        Ok [ x ]
    | Token.At ->
        let s = span p in
        let* () = expected p Token.At in
        let* name = ident p in
        Error [ Diag.error s ("unknown attribute `@" ^ name ^ "`") ]
    | t ->
        Error
          [ Diag.error (span p) ("expected a top-level item, found " ^ Token.show t) ]

  and const_item p =
    let s = span p in
    let* () = expected p Token.Kw_const in
    let* name = ident p in
    let* ty = ty p in
    let* () = expected p Token.Assign in
    skip_newlines p;
    let* value =
      if at p Token.Lbrace then
        let* () = expected p Token.Lbrace in
        delimited p (fun () ->
            let rec es acc =
              if at p Token.Rbrace then Ok (List.rev acc)
              else
                let* e = expr p in
                let* () =
                  if eat p Token.Comma then Ok ()
                  else if at p Token.Rbrace then Ok ()
                  else
                    Error
                      [ Diag.error (span p) "expected comma between literal elements" ]
                in
                es (e :: acc)
            in
            let* xs = es [] in
            let* () = expected p Token.Rbrace in
            Ok (Ast.Array_lit (xs, s)))
      else expr p
    in
    let* () = end_stmt p in
    Ok (Ast.Const { name; ty; value; span = s })

  and global_item p linkage =
    let s = span p in
    let* () = expected p Token.Kw_var in
    let* name = ident p in
    let* ty = ty p in
    let* init =
      if eat p Token.Assign then (
        skip_newlines p;
        let* value = expr p in
        Ok (Some value))
      else Ok None
    in
    let linkage =
      match (linkage, init) with
      | Ast.Internal_global, _ -> Ast.Internal_global
      | (Ast.Export_c | Ast.Import_c | Ast.Import_const_c), Some _ -> Ast.Export_c
      | (Ast.Export_c | Ast.Import_c | Ast.Import_const_c), None -> Ast.Import_c
    in
    let* () = end_stmt p in
    Ok (Ast.Global { name; ty; init; linkage; span = s })

  and struct_item p =
    let s = span p in
    let* () = expected p Token.Kw_struct in
    let* name = ident p in
    let* generic_params = generic_params p in
    let* align =
      if at p Token.At then
        let at_span = span p in
        let* () = expected p Token.At in
        let* attr_name = ident p in
        if attr_name <> "align" then
          Error [ Diag.error at_span ("unknown attribute `@" ^ attr_name ^ "`") ]
        else
          let* () = expected p Token.Lparen in
          delimited p (fun () ->
              let* n = int_value p in
              let* () = expected p Token.Rparen in
              Ok (Some n))
      else Ok None
    in
    let* () = expected p Token.Lbrace in
    skip_newlines p;
    let rec fields acc =
      if at p Token.Rbrace then Ok (List.rev acc)
      else
        let fs = span p in
        let* n = ident p in
        let* t = ty p in
        let* () =
          if eat p Token.Comma then (
            skip_newlines p;
            Ok ())
          else (
            skip_newlines p;
            Ok ())
        in
        fields
          (({
              Ast.name = n;
              ty = t;
              span = fs;
              offset = None;
              unsupported_reason = None;
            }
             : Ast.field)
          :: acc)
    in
    let* fs = fields [] in
    let* () = expected p Token.Rbrace in
    let* () = end_stmt p in
    Ok
      (Ast.Struct
         {
           name;
           generic_params;
           fields = fs;
           align;
           size = None;
           is_union = false;
           span = s;
         })

  and opaque_item p =
    let s = span p in
    let* () = expected p Token.Kw_opaque in
    let* name = ident p in
    let* () = end_stmt p in
    Ok (Ast.Opaque { name; span = s })

  and generic_params p =
    if not (at p Token.Lbracket) then Ok []
    else
      let* () = expected p Token.Lbracket in
      delimited p (fun () ->
          let rec go acc =
            let s = span p in
            let* n = ident p in
            let* next =
              if eat p Token.Kw_const then
                let* t = ty p in
                Ok (Ast.Const_param { Ast.name = n; ty = t; span = s })
              else Ok (Ast.Type_param { name = n; span = s })
            in
            if eat p Token.Comma then go (next :: acc)
            else
              let* () = expected p Token.Rbracket in
              Ok (List.rev (next :: acc))
          in
          go [])

  and signature p allow_variadic =
    let* () = expected p Token.Lparen in
    let* ps, variadic =
      delimited p (fun () ->
          let rec params acc variadic =
            if at p Token.Rparen then
              let* () = expected p Token.Rparen in
              Ok (List.rev acc, variadic)
            else if at p Token.Ellipsis then
              if not allow_variadic then
                Error [ Diag.error (span p) "`...` is legal only in extern \"C\"" ]
              else if acc = [] then
                Error
                  [
                    Diag.error (span p) "a variadic declaration needs a fixed parameter";
                  ]
              else
                let* () = expected p Token.Ellipsis in
                let* () = expected p Token.Rparen in
                Ok (List.rev acc, true)
            else
              let ps = span p in
              let* name = ident p in
              let* t = ty p in
              let param : Ast.param = { Ast.name; ty = t; span = ps } in
              ignore (eat p Token.Comma);
              params (param :: acc) variadic
          in
          params [] false)
    in
    let* ret = ty p in
    Ok (ps, ret, variadic)

  and extern_block p =
    let s = span p in
    let* () = expected p Token.Kw_extern in
    let* abi = string p in
    if abi <> "C" then Error [ Diag.error s "only extern \"C\" is supported" ]
    else
      let* () = expected p Token.Lbrace in
      skip_newlines p;
      let rec ds acc =
        if at p Token.Rbrace then
          let* () = expected p Token.Rbrace in
          let* () = end_stmt p in
          Ok (List.rev acc)
        else if at p Token.Kw_var then
          let* global = global_item p Ast.Export_c in
          ds (global :: acc)
        else
          let* x = fn_item p true in
          match x with
          | Ast.Func f -> ds (Ast.Func { f with linkage = Ast.External_c } :: acc)
          | _ -> Error [ Diag.error s "invalid extern declaration" ]
      in
      ds []

  and fn_item p allow_variadic =
    let s = span p in
    let* () = expected p Token.Kw_fn in
    let* name = ident p in
    let* generic_params = generic_params p in
    let* ps, ret, var = signature p allow_variadic in
    if allow_variadic then
      if at p Token.Lbrace then
        if var then
          Error
            [
              Diag.error (span p) "extern \"C\" function definitions cannot be variadic";
            ]
        else
          let* body = block p in
          let* () = end_stmt p in
          Ok
            (Ast.Func
               {
                 name;
                 params = ps;
                 ret;
                 body = Ast.Statements body;
                 linkage = Ast.External_c;
                 variadic = false;
                 generic_params;
                 span = s;
               })
      else
        let* () = end_stmt p in
        Ok
          (Ast.Func
             {
               name;
               params = ps;
               ret;
               body = Ast.Declaration;
               linkage = Ast.External_c;
               variadic = var;
               generic_params;
               span = s;
             })
    else
      let* body = block p in
      Ok
        (Ast.Func
           {
             name;
             params = ps;
             ret;
             body = Ast.Statements body;
             linkage = Ast.Internal;
             variadic = var;
             generic_params;
             span = s;
           })

  and block p =
    let s = span p in
    let* () = expected p Token.Lbrace in
    p.depth <- p.depth + 1;
    if p.depth + p.nesting > p.limits.Limits.max_nesting then
      Error [ Diag.error s "block nesting exceeds the configured limit" ]
    else (
      skip_newlines p;
      let rec go acc =
        if at p Token.Rbrace then (
          let* () = expected p Token.Rbrace in
          p.depth <- p.depth - 1;
          Ok (List.rev acc))
        else if at p Token.Eof then Error [ Diag.error s "unterminated block" ]
        else
          let* x = stmt p in
          skip_newlines p;
          go (x :: acc)
      in
      go [])

  and stmt p =
    match (peek p).kind with
    | Token.Kw_use when (peek_n p 1).kind = Token.String "asm" ->
        Error [ Diag.error (span p) "assembly unit must be at top level" ]
    | Token.Kw_use when List.mem_assoc (span p).Span.start_offset p.c_containers ->
        Error [ Diag.error (span p) "C container must be at top level" ]
    | Token.Kw_var ->
        Error
          [
            Diag.error (span p)
              "`var` declares globals; locals are declared as `name Type = value`";
          ]
    | Token.Ident "view" -> view_statement p true
    | Token.Kw_return ->
        let s = span p in
        ignore (bump p);
        let* x =
          if at p Token.Newline || at p Token.Semi || at p Token.Rbrace then Ok None
          else
            let* e = expr p in
            Ok (Some e)
        in
        let* () = end_stmt p in
        Ok (Ast.Return (x, s))
    | Token.Kw_if ->
        let s = span p in
        ignore (bump p);
        let* c = expr_before_block p in
        let* a = block p in
        skip_newlines p;
        let* b =
          if eat p Token.Kw_else then (
            skip_newlines p;
            if at p Token.Kw_if then
              let* x = stmt p in
              Ok (Some [ x ])
            else
              let* x = block p in
              Ok (Some x))
          else Ok None
        in
        Ok (Ast.If (c, a, b, s))
    | Token.Kw_while ->
        let s = span p in
        ignore (bump p);
        let* c = expr_before_block p in
        let* b = block p in
        Ok (Ast.While (c, b, s))
    | Token.Kw_for -> for_stmt p
    | Token.Kw_switch -> switch_stmt p
    | Token.Kw_break ->
        let s = span p in
        ignore (bump p);
        let* () = end_stmt p in
        Ok (Ast.Break s)
    | Token.Kw_continue ->
        let s = span p in
        ignore (bump p);
        let* () = end_stmt p in
        Ok (Ast.Continue s)
    | Token.Kw_defer ->
        let s = span p in
        ignore (bump p);
        let* b = block p in
        Ok (Ast.Defer (b, s))
    | Token.Lbrace ->
        let s = span p in
        let* b = block p in
        Ok (Ast.Block (b, s))
    | Token.Ident _ when starts_type p -> declaration p
    | _ -> assignment_or_expr p

  and view_statement p consume_end =
    let s = span p in
    ignore (bump p);
    let* name = ident p in
    let* () = expected p Token.Assign in
    let* place = expr p in
    let* () = finish_statement p consume_end in
    Ok (Ast.View { name; place; span = s })

  and declaration_with_end p consume_end =
    let s = span p in
    let* name = ident p in
    let* ty = ty p in
    let* init =
      if eat p Token.Assign then
        match (peek p).Token.kind with
        | Token.Ident "raw" ->
            Error
              [
                Diag.error (span p)
                  "`= raw` is no longer supported; declare `x T` without an initializer";
              ]
        | _ ->
            let* e = expr p in
            Ok (Some e)
      else Ok None
    in
    let* () = finish_statement p consume_end in
    Ok (Ast.Let { name; ty; init; span = s })

  and declaration p = declaration_with_end p true

  and target e =
    match e with
    | Ast.Ident (n, span) -> Ok (Ast.Target_ident (n, span))
    | Ast.Select (a, args, _) -> Ok (Ast.Target_select (a, args))
    | Ast.Field (a, n, span) -> Ok (Ast.Target_field (a, n, span))
    | _ -> Error [ Diag.error (Ast.expr_span e) "invalid assignment target" ]

  and assignment_or_expr_with_end p consume_end =
    let s = span p in
    let* lhs = expr p in
    match compound_op (peek p).kind with
    | Some op ->
        ignore (bump p);
        let* rhs = expr p in
        let* t = target lhs in
        let* () = finish_statement p consume_end in
        Ok (Ast.Compound_assign (t, op, rhs, s))
    | None ->
        if eat p Token.Assign then
          let* rhs = expr p in
          let* t = target lhs in
          let* () = finish_statement p consume_end in
          Ok (Ast.Assign (t, rhs, s))
        else
          let* () = finish_statement p consume_end in
          Ok (Ast.Expr_stmt (lhs, s))

  and assignment_or_expr p = assignment_or_expr_with_end p true

  and for_clause p =
    if at p Token.Kw_var then
      Error
        [
          Diag.error (span p)
            "`var` declares globals; locals are declared as `name Type = value`";
        ]
    else if at p (Token.Ident "view") then view_statement p false
    else if starts_type p then declaration_with_end p false
    else assignment_or_expr_with_end p false

  and for_stmt p =
    let s = span p in
    ignore (bump p);
    let* init =
      if at p Token.Semi then (
        ignore (bump p);
        Ok None)
      else
        let* x = for_clause p in
        let* () = expected p Token.Semi in
        Ok (Some x)
    in
    let* cond =
      if at p Token.Semi then Ok None
      else
        let* e = expr p in
        Ok (Some e)
    in
    let* () = expected p Token.Semi in
    let* step =
      if at p Token.Lbrace then Ok None
      else
        let* x = for_clause_before_block p in
        Ok (Some x)
    in
    let* body = block p in
    Ok (Ast.For (init, cond, step, body, s))

  and switch_stmt p =
    let s = span p in
    within_nesting p s "switch nesting exceeds the configured limit" (fun () ->
        ignore (bump p);
        let* scr = expr_before_block p in
        skip_newlines p;
        let* () = expected p Token.Lbrace in
        let rec cases arms default =
          skip_newlines p;
          match (peek p).kind with
          | Token.Kw_case ->
              ignore (bump p);
              let* e = expr p in
              let* () = expected p Token.Colon in
              let* b = case_body p in
              cases ((e, b) :: arms) default
          | Token.Kw_default -> (
              let default_span = span p in
              ignore (bump p);
              match default with
              | Some (first_span, _) ->
                  Error
                    [
                      Diag.error
                        ~notes:[ "first default is at " ^ Span.to_string first_span ]
                        default_span "duplicate default arm";
                    ]
              | None ->
                  let* () = expected p Token.Colon in
                  let* b = case_body p in
                  cases arms (Some (default_span, b)))
          | Token.Rbrace ->
              let* () = expected p Token.Rbrace in
              Ok (Ast.Switch (scr, List.rev arms, Option.map snd default, s))
          | _ -> Error [ Diag.error (span p) "expected case, default, or `}`" ]
        in
        cases [] None)

  and case_body p =
    skip_newlines p;
    let rec go acc =
      if at p Token.Kw_case || at p Token.Kw_default || at p Token.Rbrace then
        Ok (List.rev acc)
      else
        let* x = stmt p in
        skip_newlines p;
        go (x :: acc)
    in
    go []

  and expr_before_block p =
    let previous = p.block_expression_depth in
    p.block_expression_depth <- Some (p.depth + 1);
    let result = expr p in
    p.block_expression_depth <- previous;
    result

  and for_clause_before_block p =
    let previous = p.block_expression_depth in
    p.block_expression_depth <- Some (p.depth + 1);
    let result = for_clause p in
    p.block_expression_depth <- previous;
    result

  and expr p =
    p.depth <- p.depth + 1;
    if p.depth + p.nesting > p.limits.Limits.max_nesting then
      Error [ Diag.error (span p) "expression nesting exceeds the configured limit" ]
    else
      let r = ternary p in
      p.depth <- p.depth - 1;
      r

  and ternary p =
    let* c = or_ p in
    if eat p Token.Question then
      let s = span p in
      let* a = expr p in
      let* () = expected p Token.Colon in
      let* b = ternary p in
      Ok (Ast.Ternary (c, a, b, s))
    else Ok c

  and binary p next ops =
    let* first = next p in
    let rec go lhs =
      match (peek p).kind with
      | k when List.mem_assoc k ops ->
          let op = List.assoc k ops in
          let s = span p in
          ignore (bump p);
          let* rhs = next p in
          go (Ast.Binary (op, lhs, rhs, s))
      | _ -> Ok lhs
    in
    go first

  and or_ p = binary p and_ [ (Token.Oror, Ast.Or) ]
  and and_ p = binary p equality [ (Token.Andand, Ast.And) ]
  and bitor p = binary p bitxor [ (Token.Pipe, Ast.Bit_or) ]
  and bitxor p = binary p bitand [ (Token.Caret, Ast.Bit_xor) ]
  and bitand p = binary p shift [ (Token.Amp, Ast.Bit_and) ]
  and equality p = binary p relational [ (Token.Eqeq, Ast.Eq); (Token.Neq, Ast.Ne) ]

  and relational p =
    binary p bitor
      [ (Token.Lt, Ast.Lt); (Token.Le, Ast.Le); (Token.Gt, Ast.Gt); (Token.Ge, Ast.Ge) ]

  and shift p = binary p additive [ (Token.Ltlt, Ast.Shl); (Token.Gtgt, Ast.Shr) ]

  and additive p =
    binary p multiplicative [ (Token.Plus, Ast.Add); (Token.Minus, Ast.Sub) ]

  and multiplicative p =
    binary p unary
      [ (Token.Star, Ast.Mul); (Token.Slash, Ast.Div); (Token.Percent, Ast.Rem) ]

  and unary p =
    match (peek p).kind with
    | Token.Amp ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Addr_of (e, s)))
    | Token.Minus ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Neg, e, s)))
    | Token.Not ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Not, e, s)))
    | Token.Tilde ->
        let s = span p in
        within_nesting p s "unary nesting exceeds the configured limit" (fun () ->
            ignore (bump p);
            let* e = unary p in
            Ok (Ast.Unary (Ast.Bit_not, e, s)))
    | _ -> postfix p

  and postfix p =
    let* first = primary p in
    let brackets_before_call () =
      let rec scan offset depth =
        match (peek_n p offset).kind with
        | Token.Lbracket -> scan (offset + 1) (depth + 1)
        | Token.Rbracket when depth = 1 -> (peek_n p (offset + 1)).kind = Token.Lparen
        | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
        | Token.Newline when depth > 0 -> scan (offset + 1) depth
        | Token.Eof | Token.Newline -> false
        | _ -> scan (offset + 1) depth
      in
      scan 0 0
    in
    let rec go e =
      match (peek p).kind with
      | Token.Lparen ->
          let* xs = args p in
          let finish = p.tokens.(p.pos - 1).Token.span in
          let rec callee_start = function
            | Ast.Generic_args (callee, _, _) -> callee_start callee
            | expression -> Ast.expr_span expression
          in
          let base = callee_start e in
          let call_span =
            Span.make ~file:base.Span.file ~start_offset:base.Span.start_offset
              ~end_offset:finish.Span.end_offset ~line:base.Span.line
              ~column:base.Span.column
          in
          go (Ast.Call (e, xs, call_span))
      | Token.Lbracket when brackets_before_call () ->
          let s = span p in
          let* arguments = generic_args p in
          go (Ast.Generic_args (e, arguments, s))
      | Token.Lbracket ->
          ignore (bump p);
          let* args = select_payloads p in
          let finish = p.tokens.(p.pos - 1).Token.span in
          let base = Ast.expr_span e in
          let selection_span =
            Span.make ~file:base.Span.file ~start_offset:base.Span.start_offset
              ~end_offset:finish.Span.end_offset ~line:base.Span.line
              ~column:base.Span.column
          in
          go (Ast.Select (e, args, selection_span))
      | Token.Colon when (peek_n p 1).kind = Token.Assign ->
          Error
            [
              Diag.error (span p)
                "locals are declared as `name Type = value`; Fas has no `:=`";
            ]
      | (Token.Plus | Token.Minus) as op
        when (peek_n p 1).kind = op
             && (peek p).span.Span.end_offset = (peek_n p 1).span.Span.start_offset ->
          let name = match e with Ast.Ident (name, _) -> name | _ -> "x" in
          let operator, assignment =
            if op = Token.Plus then ("++", "+=") else ("--", "-=")
          in
          Error
            [
              Diag.error (span p)
                (Printf.sprintf "Fas has no `%s`; write `%s %s 1`" operator name
                   assignment);
            ]
      | Token.Dot ->
          let s = span p in
          ignore (bump p);
          if eat p Token.Star then
            Error
              [
                Diag.error s
                  "pointer dereference is no longer supported; use raw selection";
              ]
          else
            let* n, field_span = field_ident p in
            go (Ast.Field (e, n, field_span))
      | _ -> Ok e
    in
    go first

  and literal_elements p =
    delimited p (fun () ->
        let rec elements acc =
          if at p Token.Rbrace then
            let* () = expected p Token.Rbrace in
            Ok (List.rev acc)
          else
            let* expression = expr p in
            let* () =
              if eat p Token.Comma then Ok ()
              else if at p Token.Rbrace then Ok ()
              else
                Error [ Diag.error (span p) "expected comma between literal elements" ]
            in
            elements (expression :: acc)
        in
        elements [])

  and args p =
    let* () = expected p Token.Lparen in
    delimited p (fun () ->
        if eat p Token.Rparen then Ok []
        else
          let rec go acc =
            let* x = expr p in
            if eat p Token.Comma then go (x :: acc)
            else
              let* () = expected p Token.Rparen in
              Ok (List.rev (x :: acc))
          in
          go [])

  and type_argument p =
    let* () = expected p Token.Lbracket in
    delimited p (fun () ->
        let* t = ty p in
        let* () = expected p Token.Rbracket in
        Ok t)

  and expression_argument p =
    let* () = expected p Token.Lparen in
    delimited p (fun () ->
        let* e = expr p in
        let* () = expected p Token.Rparen in
        Ok e)

  and primary p =
    match (peek p).kind with
    | Token.Lbrace ->
        let s = span p in
        let* () = expected p Token.Lbrace in
        let* elements = literal_elements p in
        Ok (Ast.Array_lit (elements, s))
    | Token.Int s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.Int_lit (s, sp))
    | Token.String s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.String_lit (false, s, sp))
    | Token.CString s ->
        let sp = span p in
        ignore (bump p);
        Ok (Ast.String_lit (true, s, sp))
    | Token.Lparen ->
        let s = span p in
        if starts_struct_literal p then
          let* () = expected p Token.Lparen in
          let* t =
            delimited p (fun () ->
                let* t = ty p in
                let* () = expected p Token.Rparen in
                Ok t)
          in
          let* () = expected p Token.Lbrace in
          let* elements = literal_elements p in
          Ok (Ast.Struct_lit (t, elements, s))
        else
          let* e = expression_argument p in
          Ok e
    | Token.Ident n -> (
        let sp = span p in
        ignore (bump p);
        if Names.literal n = Some Names.True then Ok (Ast.Bool_lit (true, sp))
        else if Names.literal n = Some Names.False then Ok (Ast.Bool_lit (false, sp))
        else if Names.literal n = Some Names.Null then Ok (Ast.Null sp)
        else
          match (Names.parser_operation n, (peek_n p 0).kind) with
          | ( Some ((Names.Zext | Names.Sext | Names.Trunc | Names.Bitcast) as operation),
              Token.Lbracket ) ->
              let kind =
                match operation with
                | Names.Zext -> Ast.Zext
                | Names.Sext -> Ast.Sext
                | Names.Trunc -> Ast.Trunc
                | Names.Bitcast -> Ast.Bitcast
                | _ -> assert false
              in
              let* t = type_argument p in
              let* e = expression_argument p in
              Ok (Ast.Cast (kind, t, e, sp))
          | None, Token.Lbracket
            when Names.value_operation n = Some Names.Handle_from_addr ->
              let* t = type_argument p in
              let* e = expression_argument p in
              Ok (Ast.Handle_from_addr (t, e, sp))
          | Some ((Names.Sizeof | Names.Alignof | Names.Offsetof) as operation), _ ->
              let* () = expected p Token.Lbracket in
              delimited p (fun () ->
                  let* t = ty p in
                  if operation = Names.Offsetof then
                    let* () = expected p Token.Comma in
                    let* f = ident p in
                    let* () = expected p Token.Rbracket in
                    Ok (Ast.Offsetof (t, f, sp))
                  else
                    let* () = expected p Token.Rbracket in
                    Ok
                      (if operation = Names.Sizeof then Ast.Sizeof (t, sp)
                       else Ast.Alignof (t, sp)))
          | Some Names.Splat, Token.Lparen ->
              let* e = expression_argument p in
              Ok (Ast.Splat (e, sp))
          | None, Token.Lparen when n = "ptr_add" || n = "ptr_add_bytes" ->
              let message =
                if n = "ptr_add_bytes" then
                  "ptr_add_bytes is no longer supported; use address arithmetic"
                else "ptr_add is no longer supported; use address arithmetic"
              in
              Error [ Diag.error sp message ]
          | _ -> Ok (Ast.Ident (n, sp)))
    | t ->
        Error [ Diag.error (span p) ("expected an expression, found " ^ Token.show t) ]
end

let parse ?(limits = Limits.default) source =
  match extract_containers source with
  | Error e -> Error e
  | Ok (clean, c_containers) -> (
      let cleaned = Source.create ~file:(Source.file source) ~text:clean in
      match Lexer.lex ~limits cleaned with
      | Error e -> Error e
      | Ok tokens -> (
          let p =
            {
              P.tokens = Array.of_list tokens;
              pos = 0;
              c_containers;
              limits;
              depth = 0;
              nesting = 0;
              block_expression_depth = None;
              delimited_depth = 0;
            }
          in
          match P.items p with
          | Ok items -> (
              let program = { Ast.items } in
              match Ast.check_expanded_nodes ~limits program with
              | Ok () -> Ok program
              | Error diagnostic -> Error [ diagnostic ])
          | Error e -> Error e))
