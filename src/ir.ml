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
