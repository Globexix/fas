type int_kind = U8 | U16 | U32 | I8 | I16 | I32 | I64 | U64 | Usize | Isize

type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | Array of int * ty
  | Vec of int * ty
  | Struct of string
  | Opaque of string
  | Void

type field = { name : string; ty : ty; offset : int }
type struct_def = { name : string; fields : field list; size : int; align : int }
type const_def = { name : string; ty : ty; bits : int64 }
type const_arr_def = { name : string; ty : ty; elems : int64 list }

type func_sig = {
  params : (string * ty * bool * int option) list;
  ret : ty;
  variadic : bool;
}

type linkage = Internal | External_c
type attr = Ast.attr
type builtin = Shl | Lshr | Ashr | Rotl | Rotr | Popcount | Ctz | Clz
type call_target = User of string | Builtin of builtin

type expr =
  | EInt of int64 * ty * Span.t
  | EBool of bool * Span.t
  | Null of ty * Span.t
  | EString of int * Span.t
  | Local of string * ty * Span.t
  | Unary of Ast.unop * expr * ty * Span.t
  | Binary of Ast.binop * expr * expr * ty * Span.t
  | Call of call_target * expr list * ty * Span.t
  | Cast of Ast.cast_kind * expr * ty * Span.t
  | Index of expr * expr * ty * Span.t
  | Field of expr * string * ty * int * Span.t
  | Deref of expr * ty * Span.t
  | Address of expr * ty * Span.t
  | Ptr_add of bool * expr * expr * ty * Span.t
  | Sizeof of ty * int * Span.t
  | Alignof of ty * int * Span.t
  | Offsetof of ty * string * int * Span.t
  | Splat of expr * ty * Span.t
  | Ternary of expr * expr * expr * ty * Span.t
  | Const_array of string * ty * Span.t
  | Struct_lit of string * expr list * ty * Span.t

type assign_target =
  | ALocal of string
  | ADeref of expr
  | AIndex of expr * expr
  | AField of expr * string * int

type stmt =
  | Let of string * ty * expr option * Span.t
  | Assign of assign_target * expr * Span.t
  | Compound_assign of assign_target * Ast.binop * expr * ty * Span.t
  | Return of expr option * Span.t
  | If of expr * stmt list * stmt list option * Span.t
  | While of expr * stmt list * Span.t
  | For of stmt option * expr option * stmt option * stmt list * Span.t
  | Switch of expr * (expr * stmt list) list * stmt list option * Span.t
  | Break of Span.t
  | Continue of Span.t
  | Defer of stmt list * Span.t
  | Expr of expr * Span.t
  | Block of stmt list * Span.t

type func = {
  name : string;
  params : (string * ty * bool * int option) list;
  ret : ty;
  body : stmt list;
  attrs : Ast.attr list;
  linkage : linkage;
  variadic : bool;
  asm_body : string option;
}

type program = {
  structs : struct_def list;
  consts : const_def list;
  const_arrays : const_arr_def list;
  funcs : func list;
  strings : string list;
}
