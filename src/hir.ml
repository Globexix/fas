type int_kind = U8 | U16 | U32 | I8 | I16 | I32 | I64 | U64 | Usize | Isize

type ty =
  | Bool
  | Int of int_kind
  | Ptr of ty
  | ConstPtr of ty
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

let rec ty_equal a b =
  match (a, b) with
  | Bool, Bool | Void, Void -> true
  | Int a, Int b -> a = b
  | Ptr a, Ptr b -> ty_equal a b
  | ConstPtr a, ConstPtr b -> ty_equal a b
  | Array (na, a), Array (nb, b) | Vec (na, a), Vec (nb, b) -> na = nb && ty_equal a b
  | Struct a, Struct b | Opaque a, Opaque b -> a = b
  | _ -> false

let rec ty_name = function
  | Bool -> "bool"
  | Void -> "void"
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
  | Ptr t -> "ptr[" ^ ty_name t ^ "]"
  | ConstPtr t -> "ptr[const " ^ ty_name t ^ "]"
  | Array (n, t) -> Printf.sprintf "arr[%d, %s]" n (ty_name t)
  | Vec (n, t) -> Printf.sprintf "vec[%d, %s]" n (ty_name t)
  | Struct n | Opaque n -> n

let expr_ty = function
  | EInt (_, t, _)
  | Local (_, t, _)
  | Unary (_, _, t, _)
  | Binary (_, _, _, t, _)
  | Call (_, _, t, _)
  | Cast (_, _, t, _)
  | Index (_, _, t, _)
  | Field (_, _, t, _, _)
  | Deref (_, t, _)
  | Address (_, t, _)
  | Ptr_add (_, _, _, t, _)
  | Splat (_, t, _)
  | Ternary (_, _, _, t, _)
  | Const_array (_, t, _)
  | Struct_lit (_, _, t, _) ->
      t
  | EBool _ -> Bool
  | Null (t, _) -> t
  | EString _ -> ConstPtr (Int U8)
  | Sizeof _ | Alignof _ | Offsetof _ -> Int U64

let expr_span = function
  | EInt (_, _, s)
  | EBool (_, s)
  | Null (_, s)
  | EString (_, s)
  | Local (_, _, s)
  | Unary (_, _, _, s)
  | Binary (_, _, _, _, s)
  | Call (_, _, _, s)
  | Cast (_, _, _, s)
  | Index (_, _, _, s)
  | Field (_, _, _, _, s)
  | Deref (_, _, s)
  | Address (_, _, s)
  | Ptr_add (_, _, _, _, s)
  | Sizeof (_, _, s)
  | Alignof (_, _, s)
  | Offsetof (_, _, _, s)
  | Splat (_, _, s)
  | Ternary (_, _, _, _, s)
  | Const_array (_, _, s)
  | Struct_lit (_, _, _, s) ->
      s

let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x
let round_up x a = if a <= 1 then x else (x + a - 1) / a * a

let int_bytes = function
  | U8 | I8 -> 1
  | U16 | I16 -> 2
  | U32 | I32 -> 4
  | U64 | I64 | Usize | Isize -> 8

let int_layout target k = Target_layout.integer target (int_bytes k * 8)

let scalar_bits target = function
  | Bool -> Ok 1
  | Int k -> Ok (int_bytes k * 8)
  | Ptr _ | ConstPtr _ -> Ok (target.Target_layout.pointer_size * 8)
  | _ -> Error "vector element type must be a scalar"

let layout ?(target = Target_layout.current) structs ty =
  let rec go visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Ptr _ | ConstPtr _ -> Target_layout.pointer target
    | Array (n, t) ->
        let* s, a = go visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
    | Void -> Error "void has no object layout"
    | Opaque n -> Error ("opaque type `" ^ n ^ "` has no layout")
    | Struct n -> (
        if List.mem n visiting then
          Error (Printf.sprintf "recursive by-value struct `%s`" n)
        else
          match List.find_opt (fun (s : struct_def) -> s.name = n) structs with
          | None -> Error (Printf.sprintf "unknown struct `%s`" n)
          | Some (s : struct_def) -> Ok (s.size, s.align))
  in
  go [] ty

let compute_struct ?(target = Target_layout.current) decls name =
  let rec calc visiting n =
    if List.mem n visiting then
      Error (Printf.sprintf "recursive by-value struct `%s`" n)
    else
      match List.find_opt (fun (x, _, _) -> x = n) decls with
      | None -> Error (Printf.sprintf "unknown struct `%s`" n)
      | Some (_, fields, explicit) ->
          let rec each off maxa out = function
            | [] ->
                let align = max maxa (Option.value ~default:1 explicit) in
                Ok (List.rev out, round_up off align, align)
            | (fname, fty) :: rest ->
                let* size, align =
                  match fty with
                  | Struct sn ->
                      let* _, sz, al = calc (n :: visiting) sn in
                      Ok (sz, al)
                  | _ -> field_layout (n :: visiting) fty
                in
                let next = round_up off align in
                each (next + size) (max maxa align)
                  ({ name = fname; ty = fty; offset = next } :: out)
                  rest
          in
          each 0 1 [] fields
  and field_layout visiting = function
    | Bool -> Target_layout.integer target 1
    | Int k -> int_layout target k
    | Ptr _ | ConstPtr _ -> Target_layout.pointer target
    | Void -> Error "void has no object layout"
    | Opaque n -> Error (Printf.sprintf "opaque type `%s` has no layout" n)
    | Struct n ->
        let* _, s, a = calc visiting n in
        Ok (s, a)
    | Array (n, t) ->
        let* s, a = field_layout visiting t in
        let* size = Target_layout.multiply_size n s in
        Ok (size, a)
    | Vec (n, t) ->
        let* bits = scalar_bits target t in
        Target_layout.vector target n bits
  in
  let* fields, size, align = calc [] name in
  Ok { name; fields; size; align }

let render p =
  let one_struct (s : struct_def) =
    Printf.sprintf "struct %s size=%d align=%d" s.name s.size s.align
  in
  let one_fn f =
    Printf.sprintf "fn %s(%s) %s" f.name
      (String.concat ", " (List.map (fun (n, t, _, _) -> n ^ ":" ^ ty_name t) f.params))
      (ty_name f.ret)
  in
  String.concat "\n" (List.map one_struct p.structs @ List.map one_fn p.funcs)
