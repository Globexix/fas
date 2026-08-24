type ty =
  | I1
  | I8
  | I16
  | I32
  | I64
  | Ptr of ty
  | Vector of int * ty
  | Struct of string
  | Array of int * ty
  | Void

type value =
  | Const of ty * int64
  | Null of ty
  | Undef of ty
  | Zero of ty
  | Local of int * ty
  | Param of string * ty
  | Global of string * ty

type binop =
  | Add
  | Sub
  | Mul
  | Sdiv
  | Srem
  | Udiv
  | Urem
  | And
  | Or
  | Xor
  | Shl
  | Lshr
  | Ashr

type cmp = Eq | Ne | Slt | Sle | Sgt | Sge | Ult | Ule | Ugt | Uge
type gep_index = Zero | Index of value

type instr =
  | Bin of int * binop * ty * value * value
  | Cmp of int * cmp * ty * value * value
  | Alloca of int * ty * int
  | Load of int * ty * value * int
  | Store of ty * value * value * int
  | Gep of int * ty * value * gep_index list
  | Cast of int * string * ty * value * ty
  | Call of int option * ty * string * (ty * value) list
  | Phi of int * ty * (value * int) list
  | Select of int * value * value * value
  | Extract of int * ty * value * value
  | Insert of int * ty * value * value * value
  | Shuffle_zero of int * ty * value
  | String_ptr of int * int * int
  | Global_ptr of int * string * ty

type terminator =
  | Ret of (ty * value) option
  | Br of int
  | CondBr of value * int * int
  | Switch of ty * value * (int64 * int) list * int
  | Unreachable

type param = { name : string; ty : ty; noalias : bool; align : int option }
type linkage = Internal | External

type func = {
  name : string;
  params : param list;
  ret : ty;
  blocks : block list;
  linkage : linkage;
  variadic : bool;
  attrs : Ast.attr list;
  asm_body : string option;
}

and block = { id : int; label : string; instrs : instr list; terminator : terminator }

type struct_def = { name : string; fields : ty list; tail_padding : int }

type global =
  | String_global of { name : string; bytes : string }
  | Array_global of { name : string; elem_ty : ty; elems : int64 list; align : int }

type module_ = {
  target_triple : string;
  data_layout : string;
  structs : struct_def list;
  globals : global list;
  funcs : func list;
}

let rec ty_name = function
  | I1 -> "i1"
  | I8 -> "i8"
  | I16 -> "i16"
  | I32 -> "i32"
  | I64 -> "i64"
  | Ptr _ -> "ptr"
  | Vector (n, t) -> Printf.sprintf "<%d x %s>" n (ty_name t)
  | Struct n -> "%struct." ^ n
  | Array (n, t) -> Printf.sprintf "[%d x %s]" n (ty_name t)
  | Void -> "void"

let value_ty = function
  | Const (t, _)
  | Null t
  | Undef t
  | Zero t
  | Local (_, t)
  | Param (_, t)
  | Global (_, t) ->
      t

let symbol n =
  if
    String.for_all
      (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' | '$' -> true
        | _ -> false)
      n
  then "@" ^ n
  else "@\"" ^ String.escaped n ^ "\""

let value_name = function
  | Const (I1, 0L) -> "false"
  | Const (I1, _) -> "true"
  | Const (_, v) -> Int64.to_string v
  | Null _ -> "null"
  | Undef _ -> "poison"
  | Zero _ -> "zeroinitializer"
  | Local (i, _) -> Printf.sprintf "%%v%d" i
  | Param (n, _) -> "%" ^ n
  | Global (n, _) -> symbol n

let quote_bytes s =
  let b = Buffer.create (String.length s * 2) in
  String.iter
    (fun c ->
      let n = Char.code c in
      if n >= 32 && n < 127 && c <> '"' && c <> '\\' then Buffer.add_char b c
      else Buffer.add_string b (Printf.sprintf "\\%02X" n))
    s;
  Buffer.contents b

let bin_name = function
  | Add -> "add"
  | Sub -> "sub"
  | Mul -> "mul"
  | Sdiv -> "sdiv"
  | Srem -> "srem"
  | Udiv -> "udiv"
  | Urem -> "urem"
  | And -> "and"
  | Or -> "or"
  | Xor -> "xor"
  | Shl -> "shl"
  | Lshr -> "lshr"
  | Ashr -> "ashr"

let cmp_name = function
  | Eq -> "eq"
  | Ne -> "ne"
  | Slt -> "slt"
  | Sle -> "sle"
  | Sgt -> "sgt"
  | Sge -> "sge"
  | Ult -> "ult"
  | Ule -> "ule"
  | Ugt -> "ugt"
  | Uge -> "uge"
