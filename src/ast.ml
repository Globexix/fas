type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | Ptr_const of ty
  | Array of string * ty
  | Vec of string * ty
  | Named_type of string
  | Applied_type of string * generic_arg list * Span.t
  | Void

and int_kind = U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | Usize | Isize
and generic_arg = Type_arg of ty | Const_arg of expr | Name_arg of string * Span.t

and expr =
  | Int_lit of string * Span.t
  | Bool_lit of bool * Span.t
  | Null of Span.t
  | String_lit of bool * string * Span.t
  | Ident of string * Span.t
  | Unary of unop * expr * Span.t
  | Binary of binop * expr * expr * Span.t
  | Call of expr * expr list * Span.t
  | Generic_args of expr * generic_arg list * Span.t
  | Cast of cast_kind * ty * expr * Span.t
  | Index of expr * expr * Span.t
  | Field of expr * string * Span.t
  | Deref of expr * Span.t
  | Addr_of of expr * Span.t
  | Ptr_add of bool * expr * expr * Span.t
  | Sizeof of ty * Span.t
  | Alignof of ty * Span.t
  | Offsetof of ty * string * Span.t
  | Splat of expr * Span.t
  | Ternary of expr * expr * expr * Span.t
  | Array_lit of expr list * Span.t
  | Struct_lit of ty * expr list * Span.t

and unop = Neg | Not | Bit_not

and binop =
  | Add
  | Sub
  | Mul
  | Div
  | Rem
  | Bit_and
  | Bit_or
  | Bit_xor
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | And
  | Or
  | Shl
  | Shr

and cast_kind = Zext | Sext | Trunc | Bitcast

and stmt =
  | Let of { name : string; ty : ty; init : expr option; raw : bool; span : Span.t }
  | Assign of assign_target * expr * Span.t
  | Compound_assign of assign_target * binop * expr * Span.t
  | Return of expr option * Span.t
  | If of expr * stmt list * stmt list option * Span.t
  | While of expr * stmt list * Span.t
  | Break of Span.t
  | Continue of Span.t
  | Defer of stmt list * Span.t
  | Expr_stmt of expr * Span.t
  | Block of stmt list * Span.t
  | For of stmt option * expr option * stmt option * stmt list * Span.t
  | Switch of expr * (expr * stmt list) list * stmt list option * Span.t

and assign_target =
  | Target_ident of string * Span.t
  | Target_deref of expr
  | Target_index of expr * expr
  | Target_field of expr * string

and field = { name : string; ty : ty; span : Span.t }
and param = { name : string; ty : ty; span : Span.t }
and const_param = { name : string; ty : ty; span : Span.t }

and generic_param =
  | Type_param of { name : string; span : Span.t }
  | Const_param of const_param

and body = Declaration | Statements of stmt list | Asm of string
and linkage = Internal | External_c

and item =
  | Const of { name : string; ty : ty; value : expr; span : Span.t }
  | Struct of {
      name : string;
      generic_params : generic_param list;
      fields : field list;
      align : int option;
      span : Span.t;
    }
  | Opaque of { name : string; span : Span.t }
  | Func of {
      name : string;
      params : param list;
      ret : ty;
      body : body;
      linkage : linkage;
      variadic : bool;
      generic_params : generic_param list;
      span : Span.t;
    }

type program = { items : item list }

let expr_span = function
  | Int_lit (_, s)
  | Bool_lit (_, s)
  | Null s
  | String_lit (_, _, s)
  | Ident (_, s)
  | Unary (_, _, s)
  | Binary (_, _, _, s)
  | Call (_, _, s)
  | Generic_args (_, _, s)
  | Cast (_, _, _, s)
  | Index (_, _, s)
  | Field (_, _, s)
  | Deref (_, s)
  | Addr_of (_, s)
  | Ptr_add (_, _, _, s)
  | Sizeof (_, s)
  | Alignof (_, s)
  | Offsetof (_, _, s)
  | Splat (_, s)
  | Ternary (_, _, _, s)
  | Array_lit (_, s)
  | Struct_lit (_, _, s) ->
      s

let stmt_span = function
  | Let { span; _ }
  | Assign (_, _, span)
  | Compound_assign (_, _, _, span)
  | Return (_, span)
  | If (_, _, _, span)
  | While (_, _, span)
  | Break span
  | Continue span
  | Defer (_, span)
  | Expr_stmt (_, span)
  | Block (_, span)
  | For (_, _, _, _, span)
  | Switch (_, _, _, span) ->
      span

let rec type_name = function
  | Bool -> "bool"
  | Int U8 -> "u8"
  | Int U16 -> "u16"
  | Int U32 -> "u32"
  | Int U64 -> "u64"
  | Int I8 -> "i8"
  | Int I16 -> "i16"
  | Int I32 -> "i32"
  | Int I64 -> "i64"
  | Int Usize -> "usize"
  | Int Isize -> "isize"
  | Ptr t -> "ptr[" ^ type_name t ^ "]"
  | Ptr_const t -> "ptr[const " ^ type_name t ^ "]"
  | Array (n, t) -> "arr[" ^ n ^ ", " ^ type_name t ^ "]"
  | Vec (n, t) -> "vec[" ^ n ^ ", " ^ type_name t ^ "]"
  | Named_type s -> s
  | Applied_type (name, args, _) ->
      name ^ "[" ^ String.concat ", " (List.map generic_arg_name args) ^ "]"
  | Void -> "void"

and generic_arg_name = function
  | Type_arg t -> type_name t
  | Const_arg e -> expr_name e
  | Name_arg (name, _) -> name

and expr_name = function
  | Int_lit (s, _) -> s
  | Bool_lit (true, _) -> "true"
  | Bool_lit (false, _) -> "false"
  | Null _ -> "null"
  | String_lit (c, s, _) -> Printf.sprintf "%s%S" (if c then "c" else "") s
  | Ident (s, _) -> s
  | Unary (op, e, _) ->
      (match op with Neg -> "-" | Not -> "!" | Bit_not -> "~") ^ expr_name e
  | Binary (op, l, r, _) ->
      expr_name l ^ " "
      ^ (match op with
        | Add -> "+"
        | Sub -> "-"
        | Mul -> "*"
        | Div -> "/"
        | Rem -> "%"
        | Bit_and -> "&"
        | Bit_or -> "|"
        | Bit_xor -> "^"
        | Eq -> "=="
        | Ne -> "!="
        | Lt -> "<"
        | Le -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | And -> "&&"
        | Or -> "||"
        | Shl -> "<<"
        | Shr -> ">>")
      ^ " " ^ expr_name r
  | Call (f, xs, _) ->
      expr_name f ^ "(" ^ String.concat ", " (List.map expr_name xs) ^ ")"
  | Generic_args (f, xs, _) ->
      expr_name f ^ "[" ^ String.concat ", " (List.map generic_arg_name xs) ^ "]"
  | Cast (k, t, e, _) ->
      (match k with
        | Zext -> "zext"
        | Sext -> "sext"
        | Trunc -> "trunc"
        | Bitcast -> "bitcast")
      ^ "[" ^ type_name t ^ "](" ^ expr_name e ^ ")"
  | Index (a, i, _) -> expr_name a ^ "[" ^ expr_name i ^ "]"
  | Field (a, n, _) -> expr_name a ^ "." ^ n
  | Deref (e, _) -> expr_name e ^ ".*"
  | Addr_of (e, _) -> "&" ^ expr_name e
  | Ptr_add (bytes, p, o, _) ->
      (if bytes then "ptr_add_bytes" else "ptr_add")
      ^ "(" ^ expr_name p ^ ", " ^ expr_name o ^ ")"
  | Sizeof (t, _) -> "sizeof[" ^ type_name t ^ "]"
  | Alignof (t, _) -> "alignof[" ^ type_name t ^ "]"
  | Offsetof (t, f, _) -> "offsetof[" ^ type_name t ^ ", " ^ f ^ "]"
  | Splat (e, _) -> "splat(" ^ expr_name e ^ ")"
  | Ternary (c, a, b, _) -> expr_name c ^ " ? " ^ expr_name a ^ " : " ^ expr_name b
  | Array_lit (xs, _) -> "{" ^ String.concat ", " (List.map expr_name xs) ^ "}"
  | Struct_lit (t, xs, _) ->
      "(" ^ type_name t ^ "){" ^ String.concat ", " (List.map expr_name xs) ^ "}"

exception Render_exhausted of string * Span.t

type render_failure = Render_failure of string * Span.t

let item_span = function
  | Const { span; _ } | Struct { span; _ } | Opaque { span; _ } | Func { span; _ } ->
      span

let escaped_char_bytes c =
  match c with
  | '\b' | '\t' | '\n' | '\r' | '"' | '\\' -> 2
  | _ when Char.code c < 32 || Char.code c >= 127 -> 4
  | _ -> 1

let escaped_bytes s =
  let length = String.length s in
  let rec go i total =
    if i = length then total else go (i + 1) (total + escaped_char_bytes s.[i])
  in
  go 0 0

let render_bounded ~budget program =
  if budget < 0 then
    Error
      (Render_failure ("rendered AST text budget must not be negative", Span.synthetic))
  else
    let buffer = Buffer.create 4096 in
    let current = ref Span.synthetic in
    let fail single =
      let message =
        if single then
          Printf.sprintf "rendered AST node exceeds the configured limit of %d bytes"
            budget
        else
          Printf.sprintf
            "cumulative rendered AST bytes exceed the configured limit of %d bytes"
            budget
      in
      raise (Render_exhausted (message, !current))
    in
    let text s =
      if Buffer.length buffer > budget - String.length s then fail false
      else Buffer.add_string buffer s
    in
    let add_name s =
      if String.length s > budget then fail true;
      if Buffer.length buffer > budget - String.length s then fail false;
      Buffer.add_string buffer s
    in
    let add_escaped s =
      if escaped_bytes s > budget then fail true;
      let length = String.length s in
      let rec go i =
        if i < length then (
          let stop = min length (i + 256) in
          text (String.escaped (String.sub s i (stop - i)));
          go stop)
      in
      go 0
    in
    let at span emit =
      let saved = !current in
      current := span;
      emit ();
      current := saved
    in
    let emit_comma_list : 'a. ('a -> unit) -> 'a list -> unit =
     fun emit xs ->
      List.iteri
        (fun i x ->
          if i > 0 then text ", ";
          emit x)
        xs
    in
    let rec emit_ty ty =
      match ty with
      | Bool -> text "bool"
      | Int U8 -> text "u8"
      | Int U16 -> text "u16"
      | Int U32 -> text "u32"
      | Int U64 -> text "u64"
      | Int I8 -> text "i8"
      | Int I16 -> text "i16"
      | Int I32 -> text "i32"
      | Int I64 -> text "i64"
      | Int Usize -> text "usize"
      | Int Isize -> text "isize"
      | Ptr inner ->
          text "ptr[";
          emit_ty inner;
          text "]"
      | Ptr_const inner ->
          text "ptr[const ";
          emit_ty inner;
          text "]"
      | Array (n, inner) ->
          text "arr[";
          add_name n;
          text ", ";
          emit_ty inner;
          text "]"
      | Vec (n, inner) ->
          text "vec[";
          add_name n;
          text ", ";
          emit_ty inner;
          text "]"
      | Named_type name -> add_name name
      | Applied_type (name, args, _) ->
          add_name name;
          text "[";
          emit_comma_list emit_generic_arg args;
          text "]"
      | Void -> text "void"
    and emit_generic_arg = function
      | Type_arg ty -> emit_ty ty
      | Const_arg e -> emit_expr e
      | Name_arg (name, span) -> at span (fun () -> add_name name)
    and emit_generic_param = function
      | Type_param { name; span; _ } -> at span (fun () -> add_name name)
      | Const_param { name; ty; span } ->
          at span (fun () ->
              add_name name;
              text " const ";
              emit_ty ty)
    and emit_generic_params = function
      | [] -> ()
      | params ->
          text "[";
          emit_comma_list emit_generic_param params;
          text "]"
    and emit_expr e =
      at (expr_span e) (fun () ->
          match e with
          | Int_lit (s, _) -> add_name s
          | Bool_lit (true, _) -> text "true"
          | Bool_lit (false, _) -> text "false"
          | Null _ -> text "null"
          | String_lit (c, s, _) ->
              text (if c then "c\"" else "\"");
              add_escaped s;
              text "\""
          | Ident (name, _) -> add_name name
          | Unary (op, x, _) ->
              text (match op with Neg -> "-" | Not -> "!" | Bit_not -> "~");
              emit_expr x
          | Binary (op, l, r, _) ->
              emit_expr l;
              text " ";
              text
                (match op with
                | Add -> "+"
                | Sub -> "-"
                | Mul -> "*"
                | Div -> "/"
                | Rem -> "%"
                | Bit_and -> "&"
                | Bit_or -> "|"
                | Bit_xor -> "^"
                | Eq -> "=="
                | Ne -> "!="
                | Lt -> "<"
                | Le -> "<="
                | Gt -> ">"
                | Ge -> ">="
                | And -> "&&"
                | Or -> "||"
                | Shl -> "<<"
                | Shr -> ">>");
              text " ";
              emit_expr r
          | Call (f, xs, _) ->
              emit_expr f;
              text "(";
              emit_comma_list emit_expr xs;
              text ")"
          | Generic_args (f, xs, _) ->
              emit_expr f;
              text "[";
              emit_comma_list emit_generic_arg xs;
              text "]"
          | Cast (k, ty, x, _) ->
              text
                (match k with
                | Zext -> "zext["
                | Sext -> "sext["
                | Trunc -> "trunc["
                | Bitcast -> "bitcast[");
              emit_ty ty;
              text "](";
              emit_expr x;
              text ")"
          | Index (a, i, _) ->
              emit_expr a;
              text "[";
              emit_expr i;
              text "]"
          | Field (a, name, _) ->
              emit_expr a;
              text ".";
              add_name name
          | Deref (x, _) ->
              emit_expr x;
              text ".*"
          | Addr_of (x, _) ->
              text "&";
              emit_expr x
          | Ptr_add (bytes, p, o, _) ->
              text (if bytes then "ptr_add_bytes(" else "ptr_add(");
              emit_expr p;
              text ", ";
              emit_expr o;
              text ")"
          | Sizeof (ty, _) ->
              text "sizeof[";
              emit_ty ty;
              text "]"
          | Alignof (ty, _) ->
              text "alignof[";
              emit_ty ty;
              text "]"
          | Offsetof (ty, name, _) ->
              text "offsetof[";
              emit_ty ty;
              text ", ";
              add_name name;
              text "]"
          | Splat (x, _) ->
              text "splat(";
              emit_expr x;
              text ")"
          | Ternary (c, a, b, _) ->
              emit_expr c;
              text " ? ";
              emit_expr a;
              text " : ";
              emit_expr b
          | Array_lit (xs, _) ->
              text "{";
              emit_comma_list emit_expr xs;
              text "}"
          | Struct_lit (ty, xs, _) ->
              text "(";
              emit_ty ty;
              text "){";
              emit_comma_list emit_expr xs;
              text "}")
    and emit_stmt indent s =
      at (stmt_span s) (fun () ->
          match s with
          | Let { name; ty; init; raw; _ } -> (
              text indent;
              add_name name;
              text " ";
              emit_ty ty;
              if raw then text " = raw"
              else
                match init with
                | None -> ()
                | Some e ->
                    text " = ";
                    emit_expr e)
          | Assign (Target_ident (name, span), e, _) ->
              at span (fun () ->
                  text indent;
                  add_name name;
                  text " = ";
                  emit_expr e)
          | Assign (_, e, _) ->
              text indent;
              text "<target> = ";
              emit_expr e
          | Compound_assign (_, _, e, _) ->
              text indent;
              text "<target> compound= ";
              emit_expr e
          | Return (None, _) ->
              text indent;
              text "return"
          | Return (Some e, _) ->
              text indent;
              text "return ";
              emit_expr e
          | Expr_stmt (e, _) ->
              text indent;
              emit_expr e
          | Break _ ->
              text indent;
              text "break"
          | Continue _ ->
              text indent;
              text "continue"
          | Defer (xs, _) ->
              text indent;
              text "defer {";
              emit_lines (indent ^ "  ") xs;
              text "\n";
              text indent;
              text "}"
          | Block (xs, _) ->
              text indent;
              text "{";
              emit_lines (indent ^ "  ") xs;
              text "\n";
              text indent;
              text "}"
          | If (c, yes, no, _) -> (
              text indent;
              text "if ";
              emit_expr c;
              text " {";
              emit_lines (indent ^ "  ") yes;
              text "\n";
              text indent;
              text "}";
              match no with
              | None -> ()
              | Some xs ->
                  text "\n";
                  text indent;
                  text "else {";
                  emit_lines (indent ^ "  ") xs;
                  text "\n";
                  text indent;
                  text "}")
          | While (c, xs, _) ->
              text indent;
              text "while ";
              emit_expr c;
              text " {";
              emit_lines (indent ^ "  ") xs;
              text "\n";
              text indent;
              text "}"
          | For (_, _, _, xs, _) ->
              text indent;
              text "for ... {";
              emit_lines (indent ^ "  ") xs;
              text "\n";
              text indent;
              text "}"
          | Switch (e, _, _, _) ->
              text indent;
              text "switch ";
              emit_expr e;
              text " { ... }")
    and emit_lines indent xs =
      List.iter
        (fun s ->
          text "\n";
          emit_stmt indent s)
        xs
    and emit_body body =
      match body with
      | Declaration -> ()
      | Asm raw ->
          text " {";
          add_name raw;
          text "}"
      | Statements xs ->
          text " {\n";
          List.iteri
            (fun i s ->
              if i > 0 then text "\n";
              emit_stmt "  " s)
            xs;
          text "\n}"
    in
    let emit_item item =
      at (item_span item) (fun () ->
          match item with
          | Const { name; ty; value; _ } ->
              text "const ";
              add_name name;
              text " ";
              emit_ty ty;
              text " = ";
              emit_expr value
          | Struct { name; generic_params; fields; align; _ } ->
              text "struct ";
              add_name name;
              emit_generic_params generic_params;
              (match align with
              | None -> ()
              | Some n ->
                  text " @align(";
                  text (string_of_int n);
                  text ")");
              text " {\n";
              List.iteri
                (fun i f ->
                  if i > 0 then text "\n";
                  at f.span (fun () ->
                      text "  ";
                      add_name f.name;
                      text " ";
                      emit_ty f.ty))
                fields;
              text "\n}"
          | Opaque { name; _ } ->
              text "opaque ";
              add_name name
          | Func { name; generic_params; params; ret; body; linkage; _ } ->
              text (match linkage with Internal -> "fn " | External_c -> "extern fn ");
              add_name name;
              emit_generic_params generic_params;
              text "(";
              emit_comma_list
                (fun (p : param) ->
                  at p.span (fun () ->
                      add_name p.name;
                      text " ";
                      emit_ty p.ty))
                params;
              text ") ";
              emit_ty ret;
              emit_body body)
    in
    try
      let last_span = ref None in
      List.iteri
        (fun i item ->
          if i > 0 then
            at (item_span item) (fun () ->
                text "\n";
                text "\n");
          last_span := Some (item_span item);
          emit_item item)
        program.items;
      (match !last_span with
      | None -> text "\n"
      | Some span -> at span (fun () -> text "\n"));
      Ok (Buffer.contents buffer)
    with Render_exhausted (message, span) -> Error (Render_failure (message, span))

let render_program program =
  match render_bounded ~budget:max_int program with
  | Ok text -> text
  | Error _ -> assert false

let fold_expanded_nodes ~limit program =
  let total = ref 0 in
  let failed = ref None in
  let count span =
    if !failed = None then if !total >= limit then failed := Some span else incr total
  in
  let rec go_ty at ty =
    if !failed = None then
      match ty with
      | Bool | Int _ | Void | Named_type _ -> count at
      | Ptr inner | Ptr_const inner ->
          count at;
          go_ty at inner
      | Array (_, inner) | Vec (_, inner) ->
          count at;
          go_ty at inner
      | Applied_type (_, args, span) ->
          count span;
          List.iter (go_generic_arg span) args
  and go_generic_arg at = function
    | Type_arg inner ->
        count at;
        go_ty at inner
    | Const_arg e ->
        count at;
        go_expr e
    | Name_arg (_, span) -> count span
  and go_expr e =
    if !failed = None then (
      let at = expr_span e in
      count at;
      match e with
      | Int_lit _ | Bool_lit _ | Null _ | String_lit _ | Ident _ -> ()
      | Unary (_, x, _) -> go_expr x
      | Binary (_, l, r, _) ->
          go_expr l;
          go_expr r
      | Call (f, xs, _) ->
          go_expr f;
          List.iter go_expr xs
      | Generic_args (f, args, _) ->
          go_expr f;
          List.iter (go_generic_arg at) args
      | Cast (_, ty, x, _) ->
          go_ty at ty;
          go_expr x
      | Index (a, i, _) ->
          go_expr a;
          go_expr i
      | Field (a, _, _) -> go_expr a
      | Deref (x, _) | Addr_of (x, _) -> go_expr x
      | Ptr_add (_, p, o, _) ->
          go_expr p;
          go_expr o
      | Sizeof (ty, _) | Alignof (ty, _) -> go_ty at ty
      | Offsetof (ty, _, _) -> go_ty at ty
      | Splat (x, _) -> go_expr x
      | Ternary (c, a, b, _) ->
          go_expr c;
          go_expr a;
          go_expr b
      | Array_lit (xs, _) -> List.iter go_expr xs
      | Struct_lit (ty, xs, _) ->
          go_ty at ty;
          List.iter go_expr xs)
  and go_target = function
    | Target_ident (_, span) -> count span
    | Target_deref x ->
        count (expr_span x);
        go_expr x
    | Target_index (a, i) ->
        count (expr_span a);
        go_expr a;
        go_expr i
    | Target_field (a, _) ->
        count (expr_span a);
        go_expr a
  and go_stmts xs = List.iter go_stmt xs
  and go_stmt s =
    if !failed = None then (
      count (stmt_span s);
      match s with
      | Let { ty; init; span; _ } ->
          go_ty span ty;
          Option.iter go_expr init
      | Assign (t, e, _) | Compound_assign (t, _, e, _) ->
          go_target t;
          go_expr e
      | Return (e, _) -> Option.iter go_expr e
      | If (c, yes, no, _) ->
          go_expr c;
          go_stmts yes;
          Option.iter go_stmts no
      | While (c, xs, _) ->
          go_expr c;
          go_stmts xs
      | Break _ | Continue _ -> ()
      | Defer (xs, _) | Block (xs, _) -> go_stmts xs
      | Expr_stmt (e, _) -> go_expr e
      | For (i, c, st, body, _) ->
          Option.iter go_stmt i;
          Option.iter go_expr c;
          Option.iter go_stmt st;
          go_stmts body
      | Switch (scr, arms, default, _) ->
          go_expr scr;
          List.iter
            (fun (e, xs) ->
              go_expr e;
              go_stmts xs)
            arms;
          Option.iter go_stmts default)
  and go_field (f : field) =
    if !failed = None then (
      count f.span;
      go_ty f.span f.ty)
  and go_param (p : param) =
    if !failed = None then (
      count p.span;
      go_ty p.span p.ty)
  and go_generic_param = function
    | Type_param { span; _ } -> count span
    | Const_param { ty; span; _ } ->
        count span;
        go_ty span ty
  and go_item item =
    if !failed = None then (
      let at = item_span item in
      count at;
      match item with
      | Const { ty; value; span; _ } ->
          go_ty span ty;
          go_expr value
      | Struct { generic_params; fields; _ } ->
          List.iter go_generic_param generic_params;
          List.iter go_field fields
      | Opaque _ -> ()
      | Func { params; ret; body; generic_params; span; _ } -> (
          List.iter go_generic_param generic_params;
          List.iter go_param params;
          go_ty span ret;
          match body with Statements xs -> go_stmts xs | Declaration | Asm _ -> ()))
  in
  List.iter go_item program.items;
  (!total, !failed)

let count_expanded_nodes program = fst (fold_expanded_nodes ~limit:max_int program)

let check_expanded_nodes ~limits program =
  if limits.Limits.max_ast_nodes < 0 then
    Error
      (Diag.error Span.synthetic
         (Printf.sprintf "budget max_ast_nodes must not be negative (profile %s)"
            (Limits.budget_profile_name limits)))
  else
    match fold_expanded_nodes ~limit:limits.Limits.max_ast_nodes program with
    | _, None -> Ok ()
    | _, Some span ->
        Error
          (Diag.error span
             (Printf.sprintf
                "cumulative expanded AST nodes exceed budget max_ast_nodes of %d \
                 (profile %s)"
                limits.Limits.max_ast_nodes
                (Limits.budget_profile_name limits)))

let check_cumulative_asm_bytes ~limits program =
  let budget = limits.Limits.max_asm_bytes in
  if budget < 0 then
    Error
      (Diag.error Span.synthetic
         (Printf.sprintf "budget max_asm_bytes must not be negative (profile %s)"
            (Limits.budget_profile_name limits)))
  else
    let rec go total = function
      | [] -> Ok ()
      | item :: rest -> (
          match item with
          | Func { body = Asm raw; span; _ } ->
              let bytes = String.length raw in
              if bytes > budget - total then
                Error
                  (Diag.error span
                     (Printf.sprintf
                        "cumulative raw asm bytes exceed budget max_asm_bytes of %d \
                         (profile %s)"
                        budget
                        (Limits.budget_profile_name limits)))
              else go (total + bytes) rest
          | _ -> go total rest)
    in
    go 0 program.items

let specialization_name_delimiter = "$spec$"

let specialization_template_name name =
  let delimiter_length = String.length specialization_name_delimiter in
  let limit = String.length name - delimiter_length in
  let rec find index =
    if index > limit then None
    else if String.sub name index delimiter_length = specialization_name_delimiter then
      Some (String.sub name 0 index)
    else find (index + 1)
  in
  find 0

let item_span_by_name program name =
  let lookup name =
    List.find_map
      (fun item ->
        match item with
        | (Func { name = item_name; span; _ } | Const { name = item_name; span; _ })
          when item_name = name ->
            Some span
        | _ -> None)
      program.items
  in
  match lookup name with
  | Some _ as span -> span
  | None ->
      Option.fold ~none:None
        ~some:(fun template -> lookup template)
        (specialization_template_name name)
