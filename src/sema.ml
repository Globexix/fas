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

let lookup name table = List.find_opt (fun (n, _, _) -> n = name) table
let lookup_sig name c = List.assoc_opt name c.signatures

let lookup_local name c =
  let rec go = function
    | [] -> None
    | h :: t -> (
        match Hashtbl.find_opt h name with Some x -> Some x | None -> go t)
  in
  go !(c.locals)

let add_local name ty c span =
  let duplicate = Option.is_some (lookup_local name c) in
  if duplicate then error span (Printf.sprintf "duplicate local `%s`" name)
  else
    let h = match !(c.locals) with h :: _ -> h | [] -> Hashtbl.create 8 in
    Hashtbl.replace h name { ty };
    if !(c.locals) = [] then c.locals := [ h ];
    ok ()

let push c = c.locals := Hashtbl.create 8 :: !(c.locals)
let pop c = match !(c.locals) with _ :: rest -> c.locals := rest | [] -> ()

let require_init name c span =
  match lookup_local name c with
  | None -> Ok ()
  | Some b ->
      if SS.mem name c.initialized || not (is_scalar b.ty) then Ok ()
      else error span (Printf.sprintf "use of uninitialized local `%s`" name)

let mark_init name c =
  match lookup_local name c with
  | Some _b -> c.initialized <- SS.add name c.initialized
  | None -> ()

let field_info structs name field =
  match List.find_opt (fun (s : Hir.struct_def) -> s.name = name) structs with
  | None -> None
  | Some (s : Hir.struct_def) ->
      List.find_opt (fun (f : Hir.field) -> f.name = field) s.fields

let intern_string c s =
  match List.assoc_opt s c.string_ids with
  | Some i -> i
  | None ->
      let i = List.length c.strings in
      c.strings <- c.strings @ [ s ];
      c.string_ids <- (s, i) :: c.string_ids;
      i

let ptr_compatible actual expected =
  match (actual, expected) with
  | Hir.Ptr a, Hir.Ptr b -> (
      equal actual expected
      ||
      match b with
      | Hir.Int Hir.U8 -> (
          match a with Hir.Int Hir.U8 | Hir.Opaque _ -> false | _ -> true)
      | _ -> false)
  | _ -> false

let compatible actual expected =
  equal actual expected || ptr_compatible actual expected

let ensure_expected actual expected span =
  if compatible actual expected then Ok ()
  else
    error span
      (Printf.sprintf "type mismatch: expected %s, got %s" (ty_name expected)
         (ty_name actual))

let variadic_promote e =
  match Hir.expr_ty e with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, e, Hir.Int Hir.I32, Hir.expr_span e)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, e, Hir.Int Hir.I32, Hir.expr_span e)
  | _ -> e

let popcount64 x =
  let rec go n v =
    if v = 0L then n else go (n + 1) (Int64.logand v (Int64.sub v 1L))
  in
  go 0 x

let trailing64 x =
  if x = 0L then 64
  else
    let rec go n v =
      if Int64.logand v 1L <> 0L then n
      else go (n + 1) (Int64.shift_right_logical v 1)
    in
    go 0 x

let leading64 x =
  if x = 0L then 64
  else
    let rec go n bit =
      if Int64.logand x bit <> 0L then n
      else go (n + 1) (Int64.shift_right_logical bit 1)
    in
    go 0 Int64.min_int

let rec const_expr ?(structs = []) consts expected = function
  | Ast.Int_lit (raw, s) ->
      let* v =
        parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ])
      in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if not (fits_int ty v) then
        error s ("integer literal is out of range for " ^ ty_name ty)
      else Ok (ty, mask_value ty v)
  | Ast.Bool_lit (v, _) -> Ok (Hir.Bool, if v then 1L else 0L)
  | Ast.Ident (n, s) -> (
      match lookup n consts with
      | Some (_, t, v) -> Ok (t, v)
      | None -> error s "constant expression requires a known constant")
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v =
        parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ])
      in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int)
            || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_int t v || allowed then Ok (t, mask_value t (Int64.neg v))
      else error s ("integer literal is out of range for " ^ ty_name t)
  | Ast.Unary (Ast.Neg, e, s) ->
      let* t, v = const_expr ~structs consts expected e in
      if not (is_int t) then error s "unary minus requires an integer"
      else Ok (t, mask_value t (Int64.neg v))
  | Ast.Unary (Ast.Bit_not, e, s) ->
      let* t, v = const_expr ~structs consts expected e in
      if not (is_int t) then error s "bitwise not requires an integer"
      else Ok (t, mask_value t (Int64.lognot v))
  | Ast.Unary (Ast.Not, e, _) ->
      let* _, v = const_expr ~structs consts None e in
      Ok (Hir.Bool, if v = 0L then 1L else 0L)
  | Ast.Binary (op, l, r, s) ->
      let* (lt, lv), (rt, rv) =
        match (l, op) with
        | Ast.Int_lit _, (Ast.Eq | Ne | Lt | Le | Gt | Ge) ->
            let* rt, rv = const_expr ~structs consts None r in
            let* lt, lv = const_expr ~structs consts (Some rt) l in
            Ok ((lt, lv), (rt, rv))
        | _ ->
            let* lt, lv = const_expr ~structs consts expected l in
            let* rt, rv = const_expr ~structs consts (Some lt) r in
            Ok ((lt, lv), (rt, rv))
      in
      if not (equal lt rt) then error s "constant operands have different types"
      else if (op = Ast.Div || op = Ast.Rem) && rv = 0L then
        error s "division by zero in constant expression"
      else
        let cmp =
          if is_unsigned lt then Int64.unsigned_compare lv rv
          else Int64.compare lv rv
        in
        let result =
          match op with
          | Ast.Add -> Int64.add lv rv
          | Sub -> Int64.sub lv rv
          | Mul -> Int64.mul lv rv
          | Bit_and -> Int64.logand lv rv
          | Bit_or -> Int64.logor lv rv
          | Bit_xor -> Int64.logxor lv rv
          | Div ->
              if is_unsigned lt then Int64.unsigned_div lv rv
              else Int64.div lv rv
          | Rem ->
              if is_unsigned lt then Int64.unsigned_rem lv rv
              else Int64.rem lv rv
          | Eq -> if lv = rv then 1L else 0L
          | Ne -> if lv <> rv then 1L else 0L
          | Lt -> if cmp < 0 then 1L else 0L
          | Le -> if cmp <= 0 then 1L else 0L
          | Gt -> if cmp > 0 then 1L else 0L
          | Ge -> if cmp >= 0 then 1L else 0L
          | And -> if lv <> 0L && rv <> 0L then 1L else 0L
          | Or -> if lv <> 0L || rv <> 0L then 1L else 0L
        in
        let rt =
          match op with
          | Ast.Eq | Ne | Lt | Le | Gt | Ge | And | Or -> Hir.Bool
          | _ -> lt
        in
        Ok (rt, mask_value rt result)
  | Ast.Ternary (c, a, b, _s) ->
      let* _, cv = const_expr ~structs consts None c in
      if cv <> 0L then const_expr ~structs consts expected a
      else const_expr ~structs consts expected b
  | Ast.Cast (k, dst, e, s) ->
      let* dt = source_ty_diag s dst in
      let* st, v =
        const_expr ~structs consts
          (if
             k = Ast.Bitcast
             &&
             match e with
             | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
             | _ -> false
           then Some dt
           else None)
          e
      in
      let sb =
        match st with
        | Hir.Bool -> 1
        | Hir.Int q -> int_bits q
        | Hir.Ptr _ -> 64
        | _ -> 0
      in
      let result =
        match k with
        | Ast.Sext when st <> Hir.Bool && sb < 64 ->
            let shift = 64 - sb in
            Int64.shift_right (Int64.shift_left v shift) shift
        | _ -> v
      in
      Ok (dt, mask_value dt result)
  | Ast.Call (Ast.Ident (name, _), args, s) -> (
      let* vals = Result_list.map (const_expr ~structs consts expected) args in
      match (name, vals) with
      | ("shl" | "lshr" | "ashr" | "rotl" | "rotr"), [ (t, x); (_, n) ] ->
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          let k = Int64.to_int (Int64.logand n (Int64.of_int (bits - 1))) in
          let v =
            match name with
            | "shl" -> Int64.shift_left x k
            | "lshr" -> Int64.shift_right_logical x k
            | "ashr" -> Int64.shift_right x k
            | "rotl" ->
                Int64.logor (Int64.shift_left x k)
                  (Int64.shift_right_logical x (bits - k))
            | _ ->
                Int64.logor
                  (Int64.shift_right_logical x k)
                  (Int64.shift_left x (bits - k))
          in
          Ok (t, mask_value t v)
      | "popcount", [ (t, x) ] -> Ok (t, Int64.of_int (popcount64 x))
      | "ctz", [ (t, x) ] -> Ok (t, Int64.of_int (trailing64 x))
      | "clz", [ (t, x) ] ->
          let b = match t with Hir.Int q -> int_bits q | _ -> 64 in
          Ok (t, Int64.of_int (leading64 x - (64 - b)))
      | _ -> error s "invalid constant builtin call")
  | Ast.Sizeof (t, s) ->
      let* t = source_ty_diag s t in
      let* n, _ = layout_diag s structs t in
      Ok (Hir.Int Hir.U64, Int64.of_int n)
  | Ast.Alignof (t, s) ->
      let* t = source_ty_diag s t in
      let* _, n = layout_diag s structs t in
      Ok (Hir.Int Hir.U64, Int64.of_int n)
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_diag s t in
      match t with
      | Hir.Struct sn -> (
          match field_info structs sn n with
          | Some f -> Ok (Hir.Int Hir.U64, Int64.of_int f.offset)
          | None -> error s "unknown field in offsetof")
      | _ -> error s "offsetof requires a struct")
  | expr -> error (Ast.expr_span expr) "expression is not compile-time constant"
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
