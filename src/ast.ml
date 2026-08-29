type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | Ptr_const of ty
  | Array of string * ty
  | Vec of string * ty
  | Named_type of string
  | Void

and int_kind = U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | Usize | Isize

and expr =
  | Int_lit of string * Span.t
  | Bool_lit of bool * Span.t
  | Null of Span.t
  | String_lit of bool * string * Span.t
  | Ident of string * Span.t
  | Unary of unop * expr * Span.t
  | Binary of binop * expr * expr * Span.t
  | Call of expr * expr list * Span.t
  | Const_args of expr * expr list * Span.t
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

and cast_kind = Zext | Sext | Trunc | Bitcast

and stmt =
  | Let of { name : string; ty : ty; init : expr option; span : Span.t }
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
and body = Statements of stmt list | Asm of string
and linkage = Internal | External_c

and item =
  | Const of { name : string; ty : ty; value : expr; span : Span.t }
  | Struct of { name : string; fields : field list; align : int option; span : Span.t }
  | Opaque of { name : string; span : Span.t }
  | Func of {
      name : string;
      params : param list;
      ret : ty;
      body : body;
      linkage : linkage;
      variadic : bool;
      const_params : const_param list;
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
  | Const_args (_, _, s)
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
  | Void -> "void"

let rec expr_name = function
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
        | Or -> "||")
      ^ " " ^ expr_name r
  | Call (f, xs, _) ->
      expr_name f ^ "(" ^ String.concat ", " (List.map expr_name xs) ^ ")"
  | Const_args (f, xs, _) ->
      expr_name f ^ "[" ^ String.concat ", " (List.map expr_name xs) ^ "]"
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

let rec stmt_lines indent = function
  | Let { name; ty; init; _ } ->
      [
        (indent ^ name ^ " " ^ type_name ty
        ^ match init with None -> "" | Some e -> " = " ^ expr_name e);
      ]
  | Assign (Target_ident (n, _), e, _) -> [ indent ^ n ^ " = " ^ expr_name e ]
  | Assign (_, e, _) -> [ indent ^ "<target> = " ^ expr_name e ]
  | Compound_assign (_, _, e, _) -> [ indent ^ "<target> compound= " ^ expr_name e ]
  | Return (None, _) -> [ indent ^ "return" ]
  | Return (Some e, _) -> [ indent ^ "return " ^ expr_name e ]
  | Expr_stmt (e, _) -> [ indent ^ expr_name e ]
  | Break _ -> [ indent ^ "break" ]
  | Continue _ -> [ indent ^ "continue" ]
  | Defer (xs, _) ->
      ((indent ^ "defer {") :: List.concat_map (stmt_lines (indent ^ "  ")) xs)
      @ [ indent ^ "}" ]
  | Block (xs, _) ->
      ((indent ^ "{") :: List.concat_map (stmt_lines (indent ^ "  ")) xs)
      @ [ indent ^ "}" ]
  | If (c, a, b, _) -> (
      (indent ^ "if " ^ expr_name c ^ " {")
      :: List.concat_map (stmt_lines (indent ^ "  ")) a
      @ [ indent ^ "}" ]
      @
      match b with
      | None -> []
      | Some xs ->
          ((indent ^ "else {") :: List.concat_map (stmt_lines (indent ^ "  ")) xs)
          @ [ indent ^ "}" ])
  | While (c, xs, _) ->
      (indent ^ "while " ^ expr_name c ^ " {")
      :: List.concat_map (stmt_lines (indent ^ "  ")) xs
      @ [ indent ^ "}" ]
  | For (_, _, _, xs, _) ->
      ((indent ^ "for ... {") :: List.concat_map (stmt_lines (indent ^ "  ")) xs)
      @ [ indent ^ "}" ]
  | Switch (e, _, _, _) -> [ indent ^ "switch " ^ expr_name e ^ " { ... }" ]

let render_item = function
  | Const { name; ty; value; _ } ->
      "const " ^ name ^ " " ^ type_name ty ^ " = " ^ expr_name value
  | Struct { name; fields; align; _ } ->
      "struct " ^ name
      ^ (match align with None -> "" | Some n -> Printf.sprintf " @align(%d)" n)
      ^ " {\n"
      ^ String.concat "\n"
          (List.map (fun f -> "  " ^ f.name ^ " " ^ type_name f.ty) fields)
      ^ "\n}"
  | Opaque { name; _ } -> "opaque " ^ name
  | Func { name; params; ret; body; linkage; _ } -> (
      (match linkage with Internal -> "fn " | External_c -> "extern fn ")
      ^ name ^ "("
      ^ String.concat ", "
          (List.map (fun (p : param) -> p.name ^ " " ^ type_name p.ty) params)
      ^ ") " ^ type_name ret
      ^
      match body with
      | Asm raw -> " {" ^ raw ^ "}"
      | Statements xs ->
          " {\n" ^ String.concat "\n" (List.concat_map (stmt_lines "  ") xs) ^ "\n}")

let render_program program =
  String.concat "\n\n" (List.map render_item program.items) ^ "\n"
