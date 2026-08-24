type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | Array of string * ty
  | Vec of string * ty
  | Struct_type of string
  | Opaque_type of string
  | Void

and int_kind = U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 | Usize | Isize

and expr =
  | Int_lit of string * Span.t
  | Bool_lit of bool * Span.t
  | Null of Span.t
  | String_lit of string * Span.t
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
  | Struct_lit of string * expr list * Span.t

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
