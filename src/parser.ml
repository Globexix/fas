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

module P = struct
  type t = {
    tokens : Token.t array;
    mutable pos : int;
    bodies : raw_body list;
    limits : Limits.t;
    mutable depth : int;
    mutable block_expression_depth : int option;
  }

  let peek p = p.tokens.(min p.pos (Array.length p.tokens - 1))
  let peek_n p n = p.tokens.(min (p.pos + n) (Array.length p.tokens - 1))

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

  let skip_newlines p =
    while at p Token.Newline do
      ignore (bump p)
    done

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

  let rec ty p =
    match (peek p).kind with
    | Token.Ident "bool" ->
        ignore (bump p);
        Ok Ast.Bool
    | Token.Ident "void" ->
        ignore (bump p);
        Ok Ast.Void
    | Token.Ident "ptr" ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        let const = eat p Token.Kw_const in
        let* t = ty p in
        let* () = expected p Token.Rbracket in
        Ok (if const then Ast.Ptr_const t else Ast.Ptr t)
    | Token.Ident "arr" ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        let* n = aggregate_length p in
        let* () = expected p Token.Comma in
        let* t = ty p in
        let* () = expected p Token.Rbracket in
        Ok (Ast.Array (n, t))
    | Token.Ident "vec" ->
        ignore (bump p);
        let* () = expected p Token.Lbracket in
        let* n = aggregate_length p in
        let* () = expected p Token.Comma in
        let* t = ty p in
        let* () = expected p Token.Rbracket in
        Ok (Ast.Vec (n, t))
    | Token.Ident s -> (
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
        | _ ->
            if at p Token.Lbracket then
              let* args = generic_args p in
              Ok (Ast.Applied_type (s, args))
            else Ok (Ast.Named_type s))
    | t -> Error [ Diag.error (span p) ("expected a type, found " ^ Token.show t) ]

  and generic_args p =
    let* () = expected p Token.Lbracket in
    let rec go acc =
      let* arg = generic_arg p in
      if eat p Token.Comma then go (arg :: acc)
      else
        let* () = expected p Token.Rbracket in
        Ok (List.rev (arg :: acc))
    in
    go []

  and generic_arg p =
    match ((peek p).kind, (peek_n p 1).kind) with
    | Token.Ident ("sizeof" | "alignof" | "offsetof"), Token.Lbracket ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident _, Token.Lbracket when generic_application_is_call p ->
        let* e = expr p in
        Ok (Ast.Const_arg e)
    | Token.Ident name, (Token.Comma | Token.Rbracket)
      when not
             (List.mem name
                [
                  "bool";
                  "void";
                  "u8";
                  "u16";
                  "u32";
                  "u64";
                  "i8";
                  "i16";
                  "i32";
                  "i64";
                  "usize";
                  "isize";
                ]) ->
        let s = span p in
        ignore (bump p);
        Ok (Ast.Name_arg (name, s))
    | ( Token.Ident
          ( "bool" | "void" | "ptr" | "arr" | "vec" | "u8" | "u16" | "u32" | "u64"
          | "i8" | "i16" | "i32" | "i64" | "usize" | "isize" ),
        _ )
    | Token.Ident _, Token.Lbracket ->
        let* t = ty p in
        Ok (Ast.Type_arg t)
    | _ ->
        let* e = expr p in
        Ok (Ast.Const_arg e)

  and generic_application_is_call p =
    let rec scan offset depth =
      match (peek_n p offset).kind with
      | Token.Lbracket -> scan (offset + 1) (depth + 1)
      | Token.Rbracket when depth = 1 ->
          (peek_n p (offset + 1)).kind = Token.Lparen
      | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
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
      | Token.Newline | Token.Eof -> false
      | _ -> scan (offset + 1) brackets
    in
    p.block_expression_depth <> Some p.depth && scan 1 0

  and compound_op = function
    | Token.Plus_eq -> Some Ast.Add
    | Token.Minus_eq -> Some Ast.Sub
    | Token.Star_eq -> Some Ast.Mul
    | Token.Slash_eq -> Some Ast.Div
    | Token.Percent_eq -> Some Ast.Rem
    | Token.Amp_eq -> Some Ast.Bit_and
    | Token.Pipe_eq -> Some Ast.Bit_or
    | Token.Caret_eq -> Some Ast.Bit_xor
    | _ -> None

  and finish_statement p consume_end = if consume_end then end_stmt p else Ok ()

  and items p =
    skip_newlines p;
    if at p Token.Eof then Ok []
    else
      let* xs = item p in
      let* ys = items p in
      Ok (xs @ ys)

  and item p =
    match (peek p).kind with
    | Token.Kw_const ->
        let* x = const_item p in
        Ok [ x ]
    | Token.Kw_struct ->
        let* x = struct_item p in
        Ok [ x ]
    | Token.Kw_opaque ->
        let* x = opaque_item p in
        Ok [ x ]
    | Token.Kw_extern -> extern_block p
    | Token.Kw_fn ->
        let* x = fn_item p false false in
        Ok [ x ]
    | Token.Kw_asm ->
        let* x = fn_item p true false in
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
      if at p Token.Lbrace then (
        let* () = expected p Token.Lbrace in
        skip_newlines p;
        let rec es acc =
          if at p Token.Rbrace then Ok (List.rev acc)
          else
            let* e = expr p in
            let* () =
              skip_newlines p;
              if eat p Token.Comma then (
                skip_newlines p;
                Ok ())
              else if at p Token.Rbrace then Ok ()
              else
                Error [ Diag.error (span p) "expected comma between literal elements" ]
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
          let* n = int_value p in
          let* () = expected p Token.Rparen in
          Ok (Some n)
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
        fields (({ Ast.name = n; ty = t; span = fs } : Ast.field) :: acc)
    in
    let* fs = fields [] in
    let* () = expected p Token.Rbrace in
    let* () = end_stmt p in
    Ok (Ast.Struct { name; generic_params; fields = fs; align; span = s })

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
      go []

  and signature p allow_variadic =
    let* () = expected p Token.Lparen in
    skip_newlines p;
    let rec params acc variadic =
      if at p Token.Rparen then
        let* () = expected p Token.Rparen in
        let* ret = ty p in
        Ok (List.rev acc, ret, variadic)
      else if at p Token.Ellipsis then
        if not allow_variadic then
          Error [ Diag.error (span p) "`...` is legal only in extern \"C\"" ]
        else if acc = [] then
          Error [ Diag.error (span p) "a variadic declaration needs a fixed parameter" ]
        else
          let* () = expected p Token.Ellipsis in
          let* () = expected p Token.Rparen in
          let* ret = ty p in
          Ok (List.rev acc, ret, true)
      else
        let ps = span p in
        let* name = ident p in
        let* t = ty p in
        let param : Ast.param = { Ast.name; ty = t; span = ps } in
        let* () =
          if eat p Token.Comma then (
            skip_newlines p;
            Ok ())
          else Ok ()
        in
        params (param :: acc) variadic
    in
    params [] false

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
        else
          let* x = fn_item p false true in
          match x with
          | Ast.Func f -> ds (Ast.Func { f with linkage = Ast.External_c } :: acc)
          | _ -> Error [ Diag.error s "invalid extern declaration" ]
      in
      ds []

  and fn_item p asm allow_variadic =
    let s = span p in
    let* () = if asm then expected p Token.Kw_asm else Ok () in
    let* () = expected p Token.Kw_fn in
    let* name = ident p in
    let* generic_params = if asm then Ok [] else generic_params p in
    let* ps, ret, var = signature p allow_variadic in
    if asm then (
      skip_newlines p;
      let* () = expected p Token.Lbrace in
      skip_newlines p;
      let* () = expected p Token.Rbrace in
      let* () = end_stmt p in
      let raw =
        match List.find_opt (fun b -> b.name = name) p.bodies with
        | None -> ""
        | Some b -> b.text
      in
      Ok
        (Ast.Func
           {
             name;
             params = ps;
             ret;
             body = Ast.Asm raw;
             linkage = Ast.Internal;
             variadic = var;
             generic_params;
             span = s;
           }))
    else if allow_variadic then
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
    if p.depth > p.limits.Limits.max_nesting then
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

  and declaration_with_end p consume_end =
    let s = span p in
    let* name = ident p in
    let* ty = ty p in
    let* init =
      if eat p Token.Assign then
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
    | Ast.Deref (x, _) -> Ok (Ast.Target_deref x)
    | Ast.Index (a, i, _) -> Ok (Ast.Target_index (a, i))
    | Ast.Field (a, n, _) -> Ok (Ast.Target_field (a, n))
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
    if starts_type p then declaration_with_end p false
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
      | Token.Kw_default ->
          ignore (bump p);
          let* () = expected p Token.Colon in
          let* b = case_body p in
          cases arms (Some b)
      | Token.Rbrace ->
          let* () = expected p Token.Rbrace in
          Ok (Ast.Switch (scr, List.rev arms, default, s))
      | _ -> Error [ Diag.error (span p) "expected case, default, or `}`" ]
    in
    cases [] None

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
    if p.depth > p.limits.Limits.max_nesting then
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
  and and_ p = binary p bitor [ (Token.Andand, Ast.And) ]
  and bitor p = binary p bitxor [ (Token.Pipe, Ast.Bit_or) ]
  and bitxor p = binary p bitand [ (Token.Caret, Ast.Bit_xor) ]
  and bitand p = binary p equality [ (Token.Amp, Ast.Bit_and) ]
  and equality p = binary p relational [ (Token.Eqeq, Ast.Eq); (Token.Neq, Ast.Ne) ]

  and relational p =
    binary p additive
      [ (Token.Lt, Ast.Lt); (Token.Le, Ast.Le); (Token.Gt, Ast.Gt); (Token.Ge, Ast.Ge) ]

  and additive p =
    binary p multiplicative [ (Token.Plus, Ast.Add); (Token.Minus, Ast.Sub) ]

  and multiplicative p =
    binary p unary
      [ (Token.Star, Ast.Mul); (Token.Slash, Ast.Div); (Token.Percent, Ast.Rem) ]

  and unary p =
    match (peek p).kind with
    | Token.Amp ->
        let s = span p in
        ignore (bump p);
        let* e = unary p in
        Ok (Ast.Addr_of (e, s))
    | Token.Minus ->
        let s = span p in
        ignore (bump p);
        let* e = unary p in
        Ok (Ast.Unary (Ast.Neg, e, s))
    | Token.Not ->
        let s = span p in
        ignore (bump p);
        let* e = unary p in
        Ok (Ast.Unary (Ast.Not, e, s))
    | Token.Tilde ->
        let s = span p in
        ignore (bump p);
        let* e = unary p in
        Ok (Ast.Unary (Ast.Bit_not, e, s))
    | _ -> postfix p

  and postfix p =
    let* first = primary p in
    let brackets_before_call () =
      let rec scan offset depth =
        match (peek_n p offset).kind with
        | Token.Lbracket -> scan (offset + 1) (depth + 1)
        | Token.Rbracket when depth = 1 -> (peek_n p (offset + 1)).kind = Token.Lparen
        | Token.Rbracket when depth > 1 -> scan (offset + 1) (depth - 1)
        | Token.Eof | Token.Newline -> false
        | _ -> scan (offset + 1) depth
      in
      scan 0 0
    in
    let rec go e =
      match (peek p).kind with
      | Token.Lparen ->
          let s = span p in
          let* xs = args p in
          go (Ast.Call (e, xs, s))
      | Token.Lbracket when brackets_before_call () ->
          let s = span p in
          let* arguments = generic_args p in
          go (Ast.Generic_args (e, arguments, s))
      | Token.Lbracket ->
          let s = span p in
          ignore (bump p);
          let* x = expr p in
          let* () = expected p Token.Rbracket in
          go (Ast.Index (e, x, s))
      | Token.Dot ->
          let s = span p in
          ignore (bump p);
          if eat p Token.Star then go (Ast.Deref (e, s))
          else
            let* n = ident p in
            go (Ast.Field (e, n, s))
      | _ -> Ok e
    in
    go first

  and literal_elements p =
    let rec elements acc =
      if at p Token.Rbrace then
        let* () = expected p Token.Rbrace in
        Ok (List.rev acc)
      else
        let* expression = expr p in
        let* () =
          if eat p Token.Comma then Ok ()
          else if at p Token.Rbrace then Ok ()
          else Error [ Diag.error (span p) "expected comma between literal elements" ]
        in
        elements (expression :: acc)
    in
    elements []

  and args p =
    let* () = expected p Token.Lparen in
    if eat p Token.Rparen then Ok []
    else
      let rec go acc =
        let* x = expr p in
        if eat p Token.Comma then go (x :: acc)
        else
          let* () = expected p Token.Rparen in
          Ok (List.rev (x :: acc))
      in
      go []

  and primary p =
    match (peek p).kind with
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
          let* t = ty p in
          let* () = expected p Token.Rparen in
          let* () = expected p Token.Lbrace in
          let* elements = literal_elements p in
          Ok (Ast.Struct_lit (t, elements, s))
        else
          let* () = expected p Token.Lparen in
          let* e = expr p in
          let* () = expected p Token.Rparen in
          Ok e
    | Token.Ident n ->
        let sp = span p in
        ignore (bump p);
        if n = "true" then Ok (Ast.Bool_lit (true, sp))
        else if n = "false" then Ok (Ast.Bool_lit (false, sp))
        else if n = "null" then Ok (Ast.Null sp)
        else if
          (n = "zext" || n = "sext" || n = "trunc" || n = "bitcast")
          && (peek_n p 0).kind = Token.Lbracket
        then (
          let kind =
            match n with
            | "zext" -> Ast.Zext
            | "sext" -> Ast.Sext
            | "trunc" -> Ast.Trunc
            | _ -> Ast.Bitcast
          in
          ignore (bump p);
          let* t = ty p in
          let* () = expected p Token.Rbracket in
          let* () = expected p Token.Lparen in
          let* e = expr p in
          let* () = expected p Token.Rparen in
          Ok (Ast.Cast (kind, t, e, sp)))
        else if n = "sizeof" || n = "alignof" || n = "offsetof" then
          let* () = expected p Token.Lbracket in
          let* t = ty p in
          if n = "offsetof" then
            let* () = expected p Token.Comma in
            let* f = ident p in
            let* () = expected p Token.Rbracket in
            Ok (Ast.Offsetof (t, f, sp))
          else
            let* () = expected p Token.Rbracket in
            Ok (if n = "sizeof" then Ast.Sizeof (t, sp) else Ast.Alignof (t, sp))
        else if n = "splat" && at p Token.Lparen then (
          ignore (bump p);
          let* e = expr p in
          let* () = expected p Token.Rparen in
          Ok (Ast.Splat (e, sp)))
        else if (n = "ptr_add" || n = "ptr_add_bytes") && at p Token.Lparen then (
          ignore (bump p);
          let* a = expr p in
          let* () = expected p Token.Comma in
          let* b = expr p in
          let* () = expected p Token.Rparen in
          Ok (Ast.Ptr_add (n = "ptr_add_bytes", a, b, sp)))
        else Ok (Ast.Ident (n, sp))
    | t ->
        Error [ Diag.error (span p) ("expected an expression, found " ^ Token.show t) ]
end

let parse ?(limits = Limits.default) source =
  match extract_asm source limits with
  | Error e -> Error e
  | Ok (clean, bodies) -> (
      let cleaned = Source.create ~file:(Source.file source) ~text:clean in
      match Lexer.lex ~limits cleaned with
      | Error e -> Error e
      | Ok tokens -> (
          let p =
            {
              P.tokens = Array.of_list tokens;
              pos = 0;
              bodies;
              limits;
              depth = 0;
              block_expression_depth = None;
            }
          in
          match P.items p with Ok items -> Ok { Ast.items } | Error e -> Error e))
