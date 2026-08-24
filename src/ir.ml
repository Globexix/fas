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
