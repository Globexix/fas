[@@@warning "-8-26-32-34-37-39-60-69"]

module SS = Set.Make (String)

type binding = { ty : Hir.ty }

type signature = {
  params : (string * Hir.ty * bool * int option) list;
  ret : Hir.ty;
  variadic : bool;
}

type specialization = {
  item : Ast.item;
  name : string;
  values : (string * Hir.ty * int64) list;
  depth : int;
}

type trailing_args = Reject | Promote_variadic

type context = {
  structs : Hir.struct_def list;
  consts : (string * Hir.ty * int64) list;
  arrays : (string * Hir.ty * int64 list) list;
  signatures : (string * signature) list;
  templates : (string * Ast.item) list;
  specializations : specialization list ref;
  spec_depth : int;
  locals : (string, binding) Hashtbl.t list ref;
  mutable initialized : SS.t;
  mutable strings : string list;
  mutable string_ids : (string * int) list;
  ret_ty : Hir.ty;
  mutable loop_depth : int;
  mutable in_defer : bool;
  limits : Limits.t;
}

let error span message = Error [ Diag.error span message ]
let ok x = Ok x
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let src_int = function
  | Ast.U8 -> Hir.U8
  | U16 -> U16
  | U32 -> U32
  | U64 -> U64
  | I8 -> I8
  | I16 -> I16
  | I32 -> I32
  | I64 -> I64
  | Usize -> U64
  | Isize -> I64

let rec source_ty = function
  | Ast.Bool -> Ok Hir.Bool
  | Ast.Void -> Ok Hir.Void
  | Ast.Int k -> Ok (Hir.Int (src_int k))
  | Ast.Ptr t ->
      let* t = source_ty t in
      Ok (Hir.Ptr t)
  | Ast.Array (raw, t) -> source_aggregate (fun n t -> Hir.Array (n, t)) raw t
  | Ast.Vec (raw, t) -> source_aggregate (fun n t -> Hir.Vec (n, t)) raw t
  | Ast.Struct_type n -> Ok (Hir.Struct n)
  | Ast.Opaque_type n -> Ok (Hir.Opaque n)

and source_aggregate make raw element =
  try
    let length = int_of_string raw in
    if length < 0 then Error "negative aggregate length"
    else
      let* element = source_ty element in
      Ok (make length element)
  with Failure _ -> Error "aggregate length is not a machine integer"

let source_ty_diag span t =
  source_ty t |> Result.map_error (fun m -> [ Diag.error span m ])

let layout_diag span structs t =
  Hir.layout structs t |> Result.map_error (fun m -> [ Diag.error span m ])

let int_bits = function
  | Hir.U8 | I8 -> 8
  | U16 | I16 -> 16
  | U32 | I32 -> 32
  | U64 | I64 | Usize | Isize -> 64

let is_int = function Hir.Int _ -> true | _ -> false

let is_unsigned = function
  | Hir.Int (Hir.U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

let is_scalar = function Hir.Bool | Hir.Int _ | Hir.Ptr _ -> true | _ -> false

let is_numeric = function
  | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true
  | _ -> false

let is_truthy = is_scalar
let equal = Hir.ty_equal
let ty_name = Hir.ty_name

let rec object_type (structs : Hir.struct_def list) = function
  | Hir.Void -> Error "void is not an object type"
  | Hir.Opaque n ->
      Error ("opaque type `" ^ n ^ "` may only be used behind a pointer")
  | Hir.Ptr _ | Hir.Bool | Hir.Int _ -> Ok ()
  | Hir.Array (n, t) ->
      if n < 0 then Error "negative array length" else object_type structs t
  | Hir.Vec (n, t) when n > 0 -> (
      match t with
      | Hir.Int _ | Hir.Bool | Hir.Ptr _ -> Ok ()
      | _ ->
          Error
            "vector element type must be a scalar (bool, integer, or pointer)")
  | Hir.Vec _ -> Error "vector lane count must be positive"
  | Hir.Struct n ->
      if List.exists (fun (s : Hir.struct_def) -> s.name = n) structs then Ok ()
      else Error ("unknown struct `" ^ n ^ "`")

let parse_integer raw =
  let clean = String.concat "" (String.split_on_char '_' raw) in
  let radix, digits =
    if
      String.length clean > 2
      && (String.sub clean 0 2 = "0x" || String.sub clean 0 2 = "0X")
    then (16, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0b" || String.sub clean 0 2 = "0B")
    then (2, String.sub clean 2 (String.length clean - 2))
    else if
      String.length clean > 2
      && (String.sub clean 0 2 = "0o" || String.sub clean 0 2 = "0O")
    then (8, String.sub clean 2 (String.length clean - 2))
    else (10, clean)
  in
  let value = ref 0L in
  let digit c =
    match c with
    | '0' .. '9' -> Char.code c - 48
    | 'a' .. 'f' -> Char.code c - 87
    | 'A' .. 'F' -> Char.code c - 55
    | _ -> -1
  in
  if digits = "" then Error "integer literal has no digits"
  else
    let valid =
      String.for_all
        (fun c ->
          let d = digit c in
          d >= 0 && d < radix)
        digits
    in
    let limit =
      match radix with
      | 10 -> "18446744073709551615"
      | 16 -> "FFFFFFFFFFFFFFFF"
      | 8 -> "1777777777777777777777"
      | 2 -> String.make 64 '1'
      | _ -> ""
    in
    let normalized =
      if radix = 16 then String.uppercase_ascii digits else digits
    in
    let overflow =
      String.length normalized > String.length limit
      || String.length normalized = String.length limit
         && String.compare normalized limit > 0
    in
    if not valid then Error "invalid integer literal"
    else if overflow then Error "integer literal overflows 64 bits"
    else (
      String.iter
        (fun c ->
          value :=
            Int64.add
              (Int64.mul !value (Int64.of_int radix))
              (Int64.of_int (digit c)))
        digits;
      Ok !value)

let mask_value ty value =
  match ty with
  | Hir.Bool -> if value = 0L then 0L else 1L
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else Int64.logand value (Int64.sub (Int64.shift_left 1L bits) 1L)
  | Hir.Ptr _ -> value
  | _ -> value

let fits_int ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then true
      else
        let signed =
          match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
        in
        if signed then
          let range = Int64.shift_left 1L (bits - 1) in
          let min = Int64.neg range and max = Int64.sub range 1L in
          value >= min && value <= max
        else value >= 0L && value < Int64.shift_left 1L bits
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false
let unfinished name = Error [ Diag.error Span.synthetic ("sema scaffold: implement " ^ name) ]

let rec check_expr _context _expected _expression = unfinished "check_expr"
and const_key_value _ty _value = "<unfinished>"
and mangle_specialization name _values = name
and same_specialization _left _right = false
and check_call _context _expected _fn _args _span = unfinished "check_call"
and check_actuals _context _policy _span _formals _actuals = unfinished "check_actuals"

let check_target _context _target = unfinished "check_target"
let target_ty _context _target = unfinished "target_ty"

let rec stmt_must_return _statement = false
and block_must_return _body = false

let rec check_block _context _statements = unfinished "check_block"
and check_stmt _context _statement = unfinished "check_stmt"

let check ?(limits = Limits.default) (_program : Ast.program) =
  let _ = limits in
  unfinished "check"
