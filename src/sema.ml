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

let supported_targets =
  [ "x86_64"; "avx2"; "avx512"; "zen1"; "zen2"; "zen3"; "zen4"; "zen5" ]

let reserved_builtin_names =
  [ "len"; "shl"; "lshr"; "ashr"; "rotl"; "rotr"; "popcount"; "ctz"; "clz" ]

let validate_attrs span attrs =
  Result_list.iter
    (function
      | Ast.Target target when List.mem target supported_targets -> Ok ()
      | Ast.Target target ->
          error span (Printf.sprintf "unsupported target `%s`" target)
      | _ -> Ok ())
    attrs

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
  | Ast.Ptr_const t ->
      let* t = source_ty t in
      Ok (Hir.ConstPtr t)
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

let is_scalar = function
  | Hir.Bool | Hir.Int _ | Hir.Ptr _ | Hir.ConstPtr _ -> true
  | _ -> false

let is_numeric = function Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true | _ -> false
let is_truthy = is_scalar
let equal = Hir.ty_equal
let ty_name = Hir.ty_name

let rec object_type (structs : Hir.struct_def list) = function
  | Hir.Void -> Error "void is not an object type"
  | Hir.Opaque n -> Error ("opaque type `" ^ n ^ "` may only be used behind a pointer")
  | Hir.Ptr _ | Hir.ConstPtr _ | Hir.Bool | Hir.Int _ -> Ok ()
  | Hir.Array (n, t) ->
      if n < 0 then Error "negative array length" else object_type structs t
  | Hir.Vec (n, t) when n > 0 -> (
      match t with
      | Hir.Int _ | Hir.Bool | Hir.Ptr _ | Hir.ConstPtr _ -> Ok ()
      | _ -> Error "vector element type must be a scalar (bool, integer, or pointer)")
  | Hir.Vec _ -> Error "vector lane count must be positive"
  | Hir.Struct n ->
      if List.exists (fun (s : Hir.struct_def) -> s.name = n) structs then Ok ()
      else Error ("unknown struct `" ^ n ^ "`")

let aggregate_within_limit limits ty =
  let max_elements = limits.Limits.max_aggregate_elements in
  let rec check count = function
    | Hir.Array (n, t) | Hir.Vec (n, t) ->
        if n = 0 then true else n <= max_elements / count && check (count * n) t
    | _ -> count <= max_elements
  in
  check 1 ty

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
    let normalized = if radix = 16 then String.uppercase_ascii digits else digits in
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
            Int64.add (Int64.mul !value (Int64.of_int radix)) (Int64.of_int (digit c)))
        digits;
      Ok !value)

let mask_value ty value =
  match ty with
  | Hir.Bool -> if value = 0L then 0L else 1L
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else Int64.logand value (Int64.sub (Int64.shift_left 1L bits) 1L)
  | Hir.Ptr _ | Hir.ConstPtr _ -> value
  | _ -> value

let literal_limit ~negative = function
  | Hir.Int k ->
      let bits = int_bits k in
      let signed =
        match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
      in
      if signed then
        if negative then Int64.shift_left 1L (bits - 1)
        else if bits = 64 then Int64.max_int
        else Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
      else if bits = 64 then Int64.minus_one
      else Int64.sub (Int64.shift_left 1L bits) 1L
  | _ -> 0L

let fits_literal ty value =
  match ty with
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:false ty) <= 0
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false

let fits_negative_literal ty value =
  match ty with
  | Hir.Int _ -> Int64.unsigned_compare value (literal_limit ~negative:true ty) <= 0
  | Hir.Bool -> value = 0L || value = 1L
  | _ -> false

let sign_extend_bits ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      if bits = 64 then value
      else
        let mask = Int64.sub (Int64.shift_left 1L bits) 1L in
        let value = Int64.logand value mask in
        let sign_bit = Int64.shift_left 1L (bits - 1) in
        if Int64.logand value sign_bit <> 0L then Int64.logor value (Int64.lognot mask)
        else value
  | _ -> value

let sign_extend_value ty value =
  if is_int ty && not (is_unsigned ty) then sign_extend_bits ty value else value

let fits_int ty value =
  match ty with
  | Hir.Int k ->
      let bits = int_bits k in
      let signed =
        match k with Hir.I8 | I16 | I32 | I64 | Isize -> true | _ -> false
      in
      if bits = 64 then (not signed) || value >= 0L
      else if signed then
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
    | h :: t -> ( match Hashtbl.find_opt h name with Some x -> Some x | None -> go t)
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

let compatible actual expected =
  equal actual expected
  || match (actual, expected) with Hir.Ptr a, Hir.ConstPtr b -> equal a b | _ -> false

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
  let rec go n v = if v = 0L then n else go (n + 1) (Int64.logand v (Int64.sub v 1L)) in
  go 0 x

let trailing64 x =
  if x = 0L then 64
  else
    let rec go n v =
      if Int64.logand v 1L <> 0L then n else go (n + 1) (Int64.shift_right_logical v 1)
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
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if not (fits_literal ty v) then
        error s ("integer literal is out of range for " ^ ty_name ty)
      else Ok (ty, mask_value ty v)
  | Ast.Bool_lit (v, _) -> Ok (Hir.Bool, if v then 1L else 0L)
  | Ast.Ident (n, s) -> (
      match lookup n consts with
      | Some (_, t, v) -> Ok (t, v)
      | None -> error s "constant expression requires a known constant")
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then Ok (t, mask_value t (Int64.neg v))
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
        | ( (Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _)),
            (Ast.Eq | Ne | Lt | Le | Gt | Ge) ) ->
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
        let signed_lv = sign_extend_value lt lv in
        let signed_rv = sign_extend_value rt rv in
        let signed_min =
          match lt with
          | Hir.Int k ->
              let bits = int_bits k in
              if bits = 64 then Int64.min_int
              else Int64.neg (Int64.shift_left 1L (bits - 1))
          | _ -> 0L
        in
        if
          op = Ast.Div
          && (not (is_unsigned lt))
          && signed_lv = signed_min && signed_rv = Int64.minus_one
        then error s "signed division overflow in constant expression"
        else
          let cmp =
            if is_unsigned lt then Int64.unsigned_compare lv rv
            else Int64.compare signed_lv signed_rv
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
                else Int64.div signed_lv signed_rv
            | Rem ->
                if is_unsigned lt then Int64.unsigned_rem lv rv
                else Int64.rem signed_lv signed_rv
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
        | Hir.Ptr _ | Hir.ConstPtr _ -> 64
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
  | Ast.Call (Ast.Ident ("len", _), [ Ast.String_lit (_, v, _) ], s) ->
      if String.contains v '\000' then error s "string literal cannot contain NUL"
      else Ok (Hir.Int Hir.U64, Int64.of_int (String.length v))
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
            | "ashr" -> Int64.shift_right (sign_extend_bits t x) k
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
      | ("ctz" | "clz"), [ (t, 0L) ] ->
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          Ok (t, Int64.of_int bits)
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

let rec rooted_in_const_array = function
  | Hir.Const_array _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Deref (a, _, _)
  | Hir.Address (a, _, _)
  | Hir.Ptr_add (_, a, _, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_const_array a
  | Hir.Ternary (_, a, b, _, _) -> rooted_in_const_array a || rooted_in_const_array b
  | _ -> false

let rec rooted_in_string_literal = function
  | Hir.EString _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Deref (a, _, _)
  | Hir.Address (a, _, _)
  | Hir.Ptr_add (_, a, _, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_string_literal a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_string_literal a || rooted_in_string_literal b
  | _ -> false

let rec rooted_in_readonly_pointer = function
  | Hir.Deref (a, _, _) | Hir.Index (a, _, _, _) | Hir.Field (a, _, _, _, _) -> (
      match Hir.expr_ty a with
      | Hir.ConstPtr _ -> true
      | _ -> rooted_in_readonly_pointer a)
  | Hir.Address (a, _, _) | Hir.Ptr_add (_, a, _, _, _) | Hir.Cast (_, a, _, _) ->
      rooted_in_readonly_pointer a
  | Hir.Ternary (_, a, b, _, _) ->
      rooted_in_readonly_pointer a || rooted_in_readonly_pointer b
  | _ -> false

let rec check_expr (c : context) expected = function
  | Ast.Int_lit (raw, s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error s m ]) in
      let ty = Option.value ~default:(Hir.Int Hir.I32) expected in
      if not (fits_literal ty v) then
        error s ("integer literal is out of range for " ^ ty_name ty)
      else Ok (Hir.EInt (mask_value ty v, ty, s))
  | Ast.Bool_lit (v, s) -> Ok (Hir.EBool (v, s))
  | Ast.Null s -> (
      match expected with
      | Some (Hir.Ptr _ | Hir.ConstPtr _) as t -> Ok (Hir.Null (Option.get t, s))
      | _ -> error s "null requires a pointer context")
  | Ast.String_lit (cstr, v, s) ->
      if String.contains v '\000' then error s "string literal cannot contain NUL"
      else
        let value = if cstr then v ^ "\000" else v in
        Ok (Hir.EString (intern_string c value, s))
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b ->
          let* () = require_init n c s in
          Ok (Hir.Local (n, b.ty, s))
      | None -> (
          match lookup n c.consts with
          | Some (_, t, v) ->
              let rt = Option.value ~default:t expected in
              if is_int rt && fits_int rt v then
                let v =
                  match (t, rt) with
                  | Hir.Int tk, Hir.Int rk when int_bits tk < int_bits rk ->
                      sign_extend_value t v
                  | _ -> mask_value rt v
                in
                Ok (Hir.EInt (v, rt, s))
              else Ok (Hir.EInt (v, t, s))
          | None -> (
              match lookup n c.arrays with
              | Some (_, t, _) -> Ok (Hir.Const_array (n, t, s))
              | None -> error s (Printf.sprintf "unknown name `%s`" n))))
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, is), s) ->
      let* v = parse_integer raw |> Result.map_error (fun m -> [ Diag.error is m ]) in
      let t = Option.value ~default:(Hir.Int Hir.I32) expected in
      let allowed =
        match t with
        | Hir.Int ((Hir.I8 | I16 | I32 | I64 | Isize) as k) ->
            let b = int_bits k in
            (b = 64 && v = Int64.min_int) || (b < 64 && v = Int64.shift_left 1L (b - 1))
        | _ -> false
      in
      if fits_negative_literal t v || allowed then
        Ok (Hir.EInt (mask_value t (Int64.neg v), t, s))
      else error s ("integer literal is out of range for " ^ ty_name t)
  | Ast.Unary (op, e, s) -> (
      let* te =
        check_expr c
          (match op with Ast.Neg | Ast.Bit_not -> expected | Ast.Not -> None)
          e
      in
      match op with
      | Ast.Neg | Ast.Bit_not ->
          if not (is_int (Hir.expr_ty te)) then
            error s "integer unary operator requires an integer"
          else Ok (Hir.Unary (op, te, Hir.expr_ty te, s))
      | Ast.Not ->
          if not (is_truthy (Hir.expr_ty te)) then
            error s "logical not requires a scalar"
          else Ok (Hir.Unary (op, te, Hir.Bool, s)))
  | Ast.Binary (op, l, r, s) -> (
      if op = Ast.And || op = Ast.Or then
        let* a = check_expr c None l in
        let* b = check_expr c None r in
        if is_truthy (Hir.expr_ty a) && is_truthy (Hir.expr_ty b) then
          Ok (Hir.Binary (op, a, b, Hir.Bool, s))
        else error s "logical operands must be scalar"
      else
        let* a, b =
          match l with
          | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> (
              match expected with
              | Some t when is_int t ->
                  let* a = check_expr c (Some t) l in
                  let* b = check_expr c (Some t) r in
                  Ok (a, b)
              | _ ->
                  let* b = check_expr c None r in
                  let* a = check_expr c (Some (Hir.expr_ty b)) l in
                  Ok (a, b))
          | Ast.Null _ ->
              let* b = check_expr c None r in
              let* a = check_expr c (Some (Hir.expr_ty b)) l in
              Ok (a, b)
          | _ ->
              let* a = check_expr c expected l in
              let* b = check_expr c (Some (Hir.expr_ty a)) r in
              Ok (a, b)
        in
        let at = Hir.expr_ty a in
        let bt = Hir.expr_ty b in
        if
          (op = Ast.Div || op = Ast.Rem)
          && match b with Hir.EInt (v, _, _) -> v = 0L | _ -> false
        then error s "division by zero is not a defined runtime operation"
        else if
          not
            (equal at bt
            || ((op = Ast.Eq || op = Ast.Ne) && (compatible at bt || compatible bt at))
            )
        then error s "binary operands must have the same type"
        else
          match op with
          | Ast.Eq | Ast.Ne ->
              if is_scalar at then Ok (Hir.Binary (op, a, b, Hir.Bool, s))
              else error s "equality requires scalar operands"
          | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge ->
              if is_int at then Ok (Hir.Binary (op, a, b, Hir.Bool, s))
              else error s "ordered comparison requires integer operands"
          | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
          | Ast.Bit_xor ->
              if is_numeric at then Ok (Hir.Binary (op, a, b, at, s))
              else error s "arithmetic requires integer or vector operands"
          | Ast.And | Ast.Or -> error s "logical operators are handled separately")
  | Ast.Call (fn, args, s) -> check_call c None fn args s
  | Ast.Const_args (_fn, _, s) ->
      error s "const-generic specialization is not available in this checkpoint"
  | Ast.Cast (k, t, e, s) ->
      let* t = source_ty_diag s t in
      let* x =
        check_expr c
          (if
             k = Ast.Bitcast
             &&
             match e with
             | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
             | _ -> false
           then Some t
           else None)
          e
      in
      let from = Hir.expr_ty x in
      let bits = function
        | Hir.Bool -> 1
        | Hir.Int q -> int_bits q
        | Hir.Ptr _ | Hir.ConstPtr _ -> 64
        | _ -> 0
      in
      let legal =
        match k with
        | Ast.Zext ->
            (from = Hir.Bool || is_int from) && is_int t && bits t >= bits from
        | Ast.Sext ->
            (from = Hir.Bool || is_int from) && is_int t && bits t >= bits from
        | Ast.Trunc ->
            (from = Hir.Bool && is_int t)
            || (is_int from && (is_int t || t = Hir.Bool) && bits t <= bits from)
        | Ast.Bitcast -> (
            match (from, t) with
            | Hir.Ptr _, Hir.Ptr _ | Hir.Ptr _, Hir.ConstPtr _ -> true
            | Hir.ConstPtr _, Hir.ConstPtr _ -> true
            | Hir.Ptr _, Hir.Int k | Hir.Int k, Hir.Ptr _ -> int_bits k = 64
            | _ when is_int from && is_int t -> bits from = bits t
            | _ -> false)
      in
      if legal then Ok (Hir.Cast (k, x, t, s))
      else error s "illegal cast for source and destination widths"
  | Ast.Index (a, i, s) -> (
      let* ta = check_expr c None a in
      let* ti = check_expr c None i in
      if not (is_int (Hir.expr_ty ti)) then error s "array index must be an integer"
      else
        match Hir.expr_ty ta with
        | Hir.Array (_, e) | Hir.Vec (_, e) | Hir.Ptr e | Hir.ConstPtr e ->
            if match e with Hir.Opaque _ -> true | _ -> false then
              error s "opaque pointers cannot be indexed"
            else if match e with Hir.Void -> true | _ -> false then
              error s "void pointers cannot be indexed"
            else Ok (Hir.Index (ta, ti, e, s))
        | _ -> error s "cannot index this type")
  | Ast.Field (a, n, s) -> (
      let* ta = check_expr c None a in
      match Hir.expr_ty ta with
      | Hir.Struct sn -> (
          match field_info c.structs sn n with
          | Some f -> Ok (Hir.Field (ta, n, f.ty, f.offset, s))
          | None -> error s (Printf.sprintf "unknown field `%s`" n))
      | _ -> error s "field access requires a struct")
  | Ast.Deref (e, s) -> (
      let* x = check_expr c None e in
      match Hir.expr_ty x with
      | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
          error s "cannot dereference an opaque pointer"
      | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
          error s "cannot dereference a void pointer"
      | Hir.Ptr t | Hir.ConstPtr t -> Ok (Hir.Deref (x, t, s))
      | _ -> error s "cannot dereference a non-pointer")
  | Ast.Addr_of (e, s) -> (
      let* x = check_expr c None e in
      match x with
      | Hir.Local _ | Hir.Deref _ | Hir.Index _ | Hir.Field _ | Hir.Const_array _ ->
          let ty =
            if
              rooted_in_const_array x || rooted_in_string_literal x
              || rooted_in_readonly_pointer x
            then Hir.ConstPtr (Hir.expr_ty x)
            else Hir.Ptr (Hir.expr_ty x)
          in
          Ok (Hir.Address (x, ty, s))
      | _ -> error s "cannot take the address of this expression")
  | Ast.Ptr_add (bytes, p, o, s) -> (
      let* tp = check_expr c None p in
      let* toff = check_expr c None o in
      if not (is_int (Hir.expr_ty toff)) then
        error s "pointer offset must be an integer"
      else
        match Hir.expr_ty tp with
        | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
            error s "pointer arithmetic on an opaque pointer is not allowed"
        | Hir.Ptr t -> Ok (Hir.Ptr_add (bytes, tp, toff, Hir.Ptr t, s))
        | Hir.ConstPtr t -> Ok (Hir.Ptr_add (bytes, tp, toff, Hir.ConstPtr t, s))
        | _ -> error s "pointer addition requires a pointer")
  | Ast.Sizeof (t, s) ->
      let* t = source_ty_diag s t in
      let* size, _ = layout_diag s c.structs t in
      Ok (Hir.Sizeof (t, size, s))
  | Ast.Alignof (t, s) ->
      let* t = source_ty_diag s t in
      let* _, a = layout_diag s c.structs t in
      Ok (Hir.Alignof (t, a, s))
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_diag s t in
      match t with
      | Hir.Struct sn -> (
          match field_info c.structs sn n with
          | Some f -> Ok (Hir.Offsetof (t, n, f.offset, s))
          | None -> error s (Printf.sprintf "unknown field `%s`" n))
      | _ -> error s "offsetof requires a struct type")
  | Ast.Splat (e, s) -> (
      match expected with
      | Some (Hir.Vec (n, elem)) ->
          let* x = check_expr c (Some elem) e in
          if equal (Hir.expr_ty x) elem then Ok (Hir.Splat (x, Hir.Vec (n, elem), s))
          else error s "splat element type mismatch"
      | _ -> error s "splat requires a vector type context")
  | Ast.Ternary (q, a, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then
        error s "ternary condition must be scalar"
      else
        let* ta = check_expr c expected a in
        let* tb = check_expr c (Some (Hir.expr_ty ta)) b in
        if not (equal (Hir.expr_ty ta) (Hir.expr_ty tb)) then
          error s "ternary arms have different types"
        else if Hir.expr_ty ta = Hir.Void || Hir.expr_ty tb = Hir.Void then
          error s "ternary arms cannot have void type"
        else Ok (Hir.Ternary (tq, ta, tb, Hir.expr_ty ta, s))
  | Ast.Array_lit (_, s) ->
      error s "array literals are only valid in global const declarations"
  | Ast.Struct_lit (n, xs, s) -> (
      match List.find_opt (fun (d : Hir.struct_def) -> d.name = n) c.structs with
      | None -> error s (Printf.sprintf "unknown struct `%s`" n)
      | Some (d : Hir.struct_def) ->
          if List.length xs <> List.length d.fields then
            error s "wrong number of struct literal fields"
          else
            let rec go acc (fs : Hir.field list) es =
              match (fs, es) with
              | [], [] -> Ok (List.rev acc)
              | f :: ft, e :: et ->
                  let* x = check_expr c (Some f.ty) e in
                  let* () = ensure_expected (Hir.expr_ty x) f.ty (Ast.expr_span e) in
                  go (x :: acc) ft et
              | _ -> error s "wrong struct literal arity"
            in
            let* xs = go [] d.fields xs in
            Ok (Hir.Struct_lit (n, xs, Hir.Struct n, s)))

and const_key_value (t : Hir.ty) v = Hir.ty_name t ^ ":" ^ Int64.to_string v

and mangle_specialization base values =
  base ^ "$spec$"
  ^ string_of_int (String.length base)
  ^ ":"
  ^ String.concat ";"
      (List.map
         (fun (n, t, v) ->
           string_of_int (String.length n) ^ ":" ^ n ^ "=" ^ const_key_value t v)
         values)

and same_specialization a b =
  let _ = a.item in
  let _ = a.depth in
  a.name = b.name
  && List.length a.values = List.length b.values
  && List.for_all2
       (fun (n, t, v) (n2, t2, v2) -> n = n2 && equal t t2 && v = v2)
       a.values b.values

and check_call c _expected fn args s =
  match fn with
  | Ast.Const_args (Ast.Ident (name, _), cargs, _) -> (
      match List.assoc_opt name c.templates with
      | None -> error s (Printf.sprintf "unknown const-generic function `%s`" name)
      | Some (Ast.Func { params; ret; body; attrs; const_params; _ }) ->
          if List.length cargs <> List.length const_params then
            error s (Printf.sprintf "wrong number of const arguments to `%s`" name)
          else
            let rec eval acc cps actual =
              match (cps, actual) with
              | [], [] -> Ok (List.rev acc)
              | cp :: cs, a :: rest ->
                  let* ct = source_ty_diag (Ast.expr_span a) cp.Ast.ty in
                  let* vt, v = const_expr ~structs:c.structs c.consts (Some ct) a in
                  if equal vt ct then eval ((cp.name, ct, v) :: acc) cs rest
                  else error (Ast.expr_span a) "const argument type mismatch"
              | _ -> error s "const argument arity mismatch"
            in
            let* values = eval [] const_params cargs in
            if c.spec_depth >= c.limits.Limits.max_specialization_depth then
              error s "const specialization recursion depth limit exceeded"
            else if
              List.length !(c.specializations) >= c.limits.Limits.max_specializations
            then error s "const specialization count limit exceeded"
            else
              let mangled = mangle_specialization name values in
              let spec =
                {
                  item =
                    Ast.Func
                      {
                        name;
                        params;
                        ret;
                        body;
                        attrs;
                        linkage = Ast.Internal;
                        variadic = false;
                        const_params;
                        span = s;
                      };
                  name = mangled;
                  values;
                  depth = c.spec_depth;
                }
              in
              if not (List.exists (same_specialization spec) !(c.specializations)) then
                c.specializations := !(c.specializations) @ [ spec ];
              let* ps =
                Result_list.map
                  (fun (p : Ast.param) ->
                    let* t = source_ty_diag p.span p.ty in
                    Ok (p.name, t, false, None))
                  params
              in
              let* rt = source_ty_diag s ret in
              let* checked = check_actuals c Reject s ps args in
              Ok (Hir.Call (Hir.User mangled, checked, rt, s))
      | Some _ -> error s "const-generic symbol is not a function")
  | Ast.Ident ("len", _) ->
      if List.length args <> 1 then error s "builtin `len` expects one argument"
      else
        let argument = List.hd args in
        let* n =
          match argument with
          | Ast.String_lit (_, value, _) ->
              if String.contains value '\000' then
                error s "string literal cannot contain NUL"
              else Ok (String.length value)
          | _ -> (
              let* value = check_expr c None argument in
              match Hir.expr_ty value with
              | Hir.Array (n, _) -> Ok n
              | _ -> error s "len requires a fixed array or string literal")
        in
        Ok (Hir.EInt (Int64.of_int n, Hir.Int Hir.U64, s))
  | Ast.Ident (name, _) -> (
      let builtin =
        match name with
        | "shl" -> Some Hir.Shl
        | "lshr" -> Some Lshr
        | "ashr" -> Some Ashr
        | "rotl" -> Some Rotl
        | "rotr" -> Some Rotr
        | "popcount" -> Some Popcount
        | "ctz" -> Some Ctz
        | "clz" -> Some Clz
        | _ -> None
      in
      let check_builtin b =
        match b with
        | Hir.Popcount | Hir.Ctz | Hir.Clz ->
            if List.length args <> 1 then
              error s (Printf.sprintf "builtin `%s` expects one argument" name)
            else
              let* a = check_expr c None (List.hd args) in
              if is_int (Hir.expr_ty a) then
                Ok (Hir.Call (Hir.Builtin b, [ a ], Hir.expr_ty a, s))
              else error s "builtin argument must be an integer"
        | _ ->
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a = check_expr c None (List.hd args) in
              let* b2 = check_expr c None (List.hd (List.tl args)) in
              if
                (is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false)
                && is_int (Hir.expr_ty b2)
              then Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], Hir.expr_ty a, s))
              else
                error s
                  "builtin arguments must be an integer or integer vector and an \
                   integer shift"
      in
      match builtin with
      | Some b -> check_builtin b
      | None -> (
          match lookup_sig name c with
          | None -> error s (Printf.sprintf "unknown function `%s`" name)
          | Some sig_ ->
              if
                ((not sig_.variadic) && List.length args <> List.length sig_.params)
                || (sig_.variadic && List.length args < List.length sig_.params)
              then error s (Printf.sprintf "wrong number of arguments to `%s`" name)
              else
                let policy = if sig_.variadic then Promote_variadic else Reject in
                let* xs = check_actuals c policy s sig_.params args in
                Ok (Hir.Call (Hir.User name, xs, sig_.ret, s))))
  | _ -> error s "call target must be a function name"

and check_actuals c policy span formals actuals =
  let rec loop checked formals actuals =
    match (formals, actuals) with
    | [], rest ->
        let* trailing =
          match policy with
          | Reject ->
              if rest = [] then Ok [] else error span "wrong number of arguments"
          | Promote_variadic ->
              Result_list.map
                (fun expression ->
                  let* value = check_expr c None expression in
                  if is_scalar (Hir.expr_ty value) then Ok (variadic_promote value)
                  else
                    error (Ast.expr_span expression)
                      "unsupported variadic aggregate argument")
                rest
        in
        Ok (List.rev_append checked trailing)
    | (_, expected, _, _) :: formal_rest, expression :: actual_rest ->
        let* value = check_expr c (Some expected) expression in
        let* () =
          ensure_expected (Hir.expr_ty value) expected (Ast.expr_span expression)
        in
        loop (value :: checked) formal_rest actual_rest
    | _ -> error span "wrong number of arguments"
  in
  loop [] formals actuals

let check_target (c : context) = function
  | Ast.Target_ident (n, span) -> (
      match lookup_local n c with
      | Some _b -> Ok (Hir.ALocal n)
      | None -> error span (Printf.sprintf "unknown assignment target `%s`" n))
  | Ast.Target_deref e -> (
      let* x = check_expr c None e in
      if rooted_in_const_array x then
        error (Ast.expr_span e) "cannot modify const array"
      else
        match Hir.expr_ty x with
        | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
            error (Ast.expr_span e) "cannot dereference opaque pointer"
        | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
            error (Ast.expr_span e) "cannot dereference a void pointer"
        | Hir.Ptr _ -> Ok (Hir.ADeref x)
        | Hir.ConstPtr _ -> error (Ast.expr_span e) "cannot modify read-only pointer"
        | _ -> error (Ast.expr_span e) "deref assignment requires pointer")
  | Ast.Target_index (a, i) -> (
      let* x = check_expr c None a in
      let* y = check_expr c None i in
      if rooted_in_const_array x then
        error (Ast.expr_span a) "cannot modify const array"
      else if not (is_int (Hir.expr_ty y)) then
        error (Ast.expr_span i) "index must be integer"
      else
        match Hir.expr_ty x with
        | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
            error (Ast.expr_span a) "void pointers cannot be indexed"
        | Hir.Array (_, _) | Hir.Vec (_, _) | Hir.Ptr _ -> Ok (Hir.AIndex (x, y))
        | Hir.ConstPtr _ -> error (Ast.expr_span a) "cannot modify read-only pointer"
        | _ -> error (Ast.expr_span a) "index assignment requires aggregate or pointer")
  | Ast.Target_field (a, n) -> (
      let* x = check_expr c None a in
      if rooted_in_const_array x then
        error (Ast.expr_span a) "cannot modify const array"
      else if rooted_in_readonly_pointer x then
        error (Ast.expr_span a) "cannot modify read-only pointer"
      else
        match Hir.expr_ty x with
        | Hir.Struct sn -> (
            match field_info c.structs sn n with
            | Some f -> Ok (Hir.AField (x, n, f.offset))
            | None -> error (Ast.expr_span a) (Printf.sprintf "unknown field `%s`" n))
        | _ -> error (Ast.expr_span a) "field assignment requires struct")

let target_ty c = function
  | Hir.ALocal name -> Option.map (fun binding -> binding.ty) (lookup_local name c)
  | Hir.ADeref expression -> (
      match Hir.expr_ty expression with
      | Hir.Ptr t | Hir.ConstPtr t -> Some t
      | _ -> None)
  | Hir.AIndex (expression, _) -> (
      match Hir.expr_ty expression with
      | Hir.Array (_, t) | Hir.Vec (_, t) | Hir.Ptr t | Hir.ConstPtr t -> Some t
      | _ -> None)
  | Hir.AField (expression, name, _) -> (
      match Hir.expr_ty expression with
      | Hir.Struct struct_name ->
          Option.map
            (fun (field : Hir.field) -> field.ty)
            (field_info c.structs struct_name name)
      | _ -> None)

let rec stmt_must_return = function
  | Hir.Return _ -> true
  | Hir.Block (body, _) -> block_must_return body
  | Hir.If (_, then_body, Some else_body, _) ->
      block_must_return then_body && block_must_return else_body
  | Hir.Switch (_, arms, Some default, _) ->
      block_must_return default
      && List.for_all (fun (_, body) -> block_must_return body) arms
  | _ -> false

and block_must_return body = List.exists stmt_must_return body

let rec check_block (c : context) stmts =
  push c;
  let rec go acc = function
    | [] ->
        let out = List.rev acc in
        pop c;
        Ok out
    | s :: rest ->
        let* x = check_stmt c s in
        go (x :: acc) rest
  in
  go [] stmts

and check_stmt (c : context) = function
  | Ast.Let { name; ty; init; span } ->
      let* () =
        if
          Option.is_some (lookup name c.consts) || Option.is_some (lookup name c.arrays)
        then error span (Printf.sprintf "local `%s` shadows a const" name)
        else Ok ()
      in
      let* t = source_ty_diag span ty in
      let* () =
        object_type c.structs t |> Result.map_error (fun m -> [ Diag.error span m ])
      in
      let* () =
        if aggregate_within_limit c.limits t then Ok ()
        else error span "aggregate element count exceeds the configured limit"
      in
      let* () = add_local name t c span in
      let* x =
        match init with
        | None -> Ok None
        | Some e ->
            let* v = check_expr c (Some t) e in
            let* () = ensure_expected (Hir.expr_ty v) t (Ast.expr_span e) in
            mark_init name c;
            Ok (Some v)
      in
      Ok (Hir.Let (name, t, x, span))
  | Ast.Assign (t, e, span) ->
      let* target = check_target c t in
      let* expected =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid assignment target"
      in
      let* v = check_expr c (Some expected) e in
      let* () = ensure_expected (Hir.expr_ty v) expected span in
      (match target with Hir.ALocal n -> mark_init n c | _ -> ());
      Ok (Hir.Assign (target, v, span))
  | Ast.Compound_assign (t, op, e, span) ->
      let* target = check_target c t in
      let* et =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid compound assignment target"
      in
      let* () =
        match target with Hir.ALocal n -> require_init n c span | _ -> Ok ()
      in
      let* v = check_expr c (Some et) e in
      let* () = ensure_expected (Hir.expr_ty v) et span in
      if not (is_numeric et) then
        error span "compound assignment requires an integer or vector"
      else Ok (Hir.Compound_assign (target, op, v, et, span))
  | Ast.Return (e, span) ->
      let* () =
        if c.in_defer then error span "return is not allowed inside defer" else Ok ()
      in
      let* x =
        match (e, c.ret_ty) with
        | None, Hir.Void -> Ok None
        | Some _, Hir.Void -> error span "void function cannot return a value"
        | Some e, t ->
            let* v = check_expr c (Some t) e in
            let* () = ensure_expected (Hir.expr_ty v) t span in
            Ok (Some v)
        | None, _ ->
            error span ("return value required (expected " ^ ty_name c.ret_ty ^ ")")
      in
      Ok (Hir.Return (x, span))
  | Ast.Expr_stmt (e, s) ->
      let* x = check_expr c None e in
      Ok (Hir.Expr (x, s))
  | Ast.Block (xs, s) ->
      let* x = check_block c xs in
      Ok (Hir.Block (x, s))
  | Ast.If (q, a, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then error s "if condition must be scalar"
      else
        let before = c.initialized in
        let* ta = check_block c a in
        let ia = c.initialized in
        c.initialized <- before;
        let* tb =
          match b with
          | None -> Ok None
          | Some xs ->
              let* x = check_block c xs in
              Ok (Some x)
        in
        let ib = c.initialized in
        c.initialized <- SS.inter ia ib;
        Ok (Hir.If (tq, ta, tb, s))
  | Ast.While (q, b, s) ->
      let* tq = check_expr c None q in
      if not (is_truthy (Hir.expr_ty tq)) then error s "while condition must be scalar"
      else
        let before = c.initialized in
        c.loop_depth <- c.loop_depth + 1;
        let checked = check_block c b in
        c.loop_depth <- c.loop_depth - 1;
        let* tb = checked in
        c.initialized <- before;
        Ok (Hir.While (tq, tb, s))
  | Ast.For (i, q, step, b, s) ->
      let* ti =
        match i with
        | None -> Ok None
        | Some x ->
            let* y = check_stmt c x in
            Ok (Some y)
      in
      let* tq =
        match q with
        | None -> Ok None
        | Some x ->
            let* y = check_expr c None x in
            if is_truthy (Hir.expr_ty y) then Ok (Some y)
            else error (Ast.expr_span x) "for condition must be scalar"
      in
      let before = c.initialized in
      c.loop_depth <- c.loop_depth + 1;
      let body_result = check_block c b in
      let* tb = body_result in
      c.initialized <- before;
      let* ts =
        match step with
        | None -> Ok None
        | Some x ->
            let* y = check_stmt c x in
            Ok (Some y)
      in
      c.initialized <- before;
      c.loop_depth <- c.loop_depth - 1;
      Ok (Hir.For (ti, tq, ts, tb, s))
  | Ast.Switch (e, arms, d, s) ->
      let* te = check_expr c None e in
      let et = Hir.expr_ty te in
      if not (is_int et || et = Hir.Bool) then
        error s "switch scrutinee must be an integer or bool"
      else
        let before = c.initialized and seen = ref [] and branch_states = ref [] in
        let rec ar acc = function
          | [] -> Ok (List.rev acc)
          | (k, b) :: xs ->
              let* kt, kv =
                const_expr ~structs:c.structs c.consts (Some et) k
                |> Result.map_error (fun _ ->
                    [
                      Diag.error (Ast.expr_span k)
                        "case label must be a compile-time constant";
                    ])
              in
              let* () = ensure_expected kt et (Ast.expr_span k) in
              if List.mem kv !seen then
                error (Ast.expr_span k) (Printf.sprintf "duplicate case label `%Ld`" kv)
              else (
                seen := kv :: !seen;
                let tk =
                  match et with
                  | Hir.Bool -> Hir.EBool (kv <> 0L, Ast.expr_span k)
                  | _ -> Hir.EInt (mask_value et kv, et, Ast.expr_span k)
                in
                c.initialized <- before;
                let* tb = check_block c b in
                branch_states := c.initialized :: !branch_states;
                ar ((tk, tb) :: acc) xs)
        in
        let result = ar [] arms in
        let* ta = result in
        c.initialized <- before;
        let* td =
          match d with
          | None -> Ok None
          | Some x ->
              let* y = check_block c x in
              branch_states := c.initialized :: !branch_states;
              Ok (Some y)
        in
        (match d with
        | None -> branch_states := before :: !branch_states
        | Some _ -> ());
        c.initialized <-
          (match !branch_states with
          | [] -> before
          | first :: rest -> List.fold_left SS.inter first rest);
        Ok (Hir.Switch (te, ta, td, s))
  | Ast.Break s ->
      if c.in_defer then error s "break is not allowed inside defer"
      else if c.loop_depth = 0 then error s "break outside loop"
      else Ok (Hir.Break s)
  | Ast.Continue s ->
      if c.in_defer then error s "continue is not allowed inside defer"
      else if c.loop_depth = 0 then error s "continue outside loop"
      else Ok (Hir.Continue s)
  | Ast.Defer (xs, s) ->
      if c.in_defer then error s "nested defer is not allowed"
      else
        let before = c.initialized in
        c.in_defer <- true;
        let checked = check_block c xs in
        c.in_defer <- false;
        c.initialized <- before;
        let* body = checked in
        Ok (Hir.Defer (body, s))

let check ?(limits = Limits.default) program =
  let _ = limits in
  let rec collect_structs seen acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Opaque { name; span } :: rest ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else collect_structs (name :: seen) acc rest
    | Ast.Struct { name; fields; align; span } :: rest ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          let* () =
            match align with
            | Some a when a <= 0 || a land (a - 1) <> 0 ->
                error span "alignment must be a positive power of two"
            | _ -> Ok ()
          in
          let rec collect_fields fseen out = function
            | [] -> Ok (List.rev out)
            | (f : Ast.field) :: fs ->
                if List.mem f.name fseen then
                  error f.span (Printf.sprintf "duplicate field `%s`" f.name)
                else
                  let* t =
                    source_ty f.ty
                    |> Result.map_error (fun m -> [ Diag.error f.span m ])
                  in
                  collect_fields (f.name :: fseen) ((f.name, t) :: out) fs
          in
          let* fs = collect_fields [] [] fields in
          collect_structs (name :: seen) ((name, fs, align) :: acc) rest
    | _ :: rest -> collect_structs seen acc rest
  in
  let* structs_src = collect_structs [] [] program.Ast.items in
  let struct_names = List.map (fun (n, _, _) -> n) structs_src in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Opaque { name; span } when List.mem name struct_names ->
            error span (Printf.sprintf "duplicate type `%s`" name)
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let rec build acc = function
    | [] -> Ok (List.rev acc)
    | (name, _, _) :: xs ->
        let* s =
          Hir.compute_struct structs_src name
          |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
        in
        build (s :: acc) xs
  in
  let* structs = build [] structs_src in
  let source_obj t =
    let* t =
      source_ty t |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    let* () =
      object_type structs t
      |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
    in
    if aggregate_within_limit limits t then Ok t
    else error Span.synthetic "aggregate element count exceeds the configured limit"
  in
  let map_params convert params =
    Result_list.map
      (fun (param : Ast.param) ->
        let* ty = convert param in
        Ok (param.name, ty, false, None))
      params
  in
  let source_params =
    map_params (fun (param : Ast.param) -> source_ty_diag param.span param.ty)
  in
  let consts = ref [] and arrays = ref [] in
  let eval_const_item = function
    | Ast.Const { name; ty; value; span } -> (
        if
          List.exists (fun (n, _, _) -> n = name) !consts
          || List.exists (fun (n, _, _) -> n = name) !arrays
        then error span (Printf.sprintf "duplicate const `%s`" name)
        else
          let* t = source_obj ty in
          match (t, value) with
          | Hir.Array (n, elem), Ast.Array_lit (xs, _) ->
              if List.length xs <> n then error span "const array length mismatch"
              else
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | x :: rest ->
                      let* vt, v = const_expr ~structs !consts (Some elem) x in
                      if equal vt elem then values (v :: acc) rest
                      else error (Ast.expr_span x) "const array element type mismatch"
                in
                let* vs = values [] xs in
                arrays := !arrays @ [ (name, t, vs) ];
                Ok ()
          | Hir.Array _, _ -> error span "const array needs a brace-list initializer"
          | _, Ast.Array_lit _ -> error span "brace-list requires an array type"
          | _, _ ->
              let* vt, v = const_expr ~structs !consts (Some t) value in
              if equal vt t then (
                consts := !consts @ [ (name, t, v) ];
                Ok ())
              else error span "constant initializer type mismatch")
    | _ -> Ok ()
  in
  let* () = Result_list.iter eval_const_item program.items in
  let sigs = ref [] in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Func { name; params; ret; attrs; variadic; linkage; span; _ } ->
            let* () =
              if List.mem name reserved_builtin_names then
                error span
                  (Printf.sprintf
                     "`%s` is a reserved builtin name and cannot be used as a function name"
                     name)
              else Ok ()
            in
            let* () = validate_attrs span attrs in
            if List.exists (fun (n, _) -> n = name) !sigs then
              error span (Printf.sprintf "duplicate function `%s`" name)
            else if List.exists (fun (n, _, _) -> n = name) !arrays then
              error span (Printf.sprintf "duplicate declaration `%s`" name)
            else
              let rec dup seen = function
                | [] -> None
                | (p : Ast.param) :: xs ->
                    if List.mem p.name seen then Some p else dup (p.name :: seen) xs
              in
              let* () =
                match dup [] params with
                | Some p ->
                    error p.span (Printf.sprintf "duplicate parameter `%s`" p.name)
                | None -> Ok ()
              in
              let* ps =
                map_params (fun (param : Ast.param) -> source_obj param.ty) params
              in
              let* rt = source_ty_diag span ret in
              if variadic && linkage <> Ast.External_c then
                error span "variadic functions require extern \"C\""
              else (
                sigs := !sigs @ [ (name, { params = ps; ret = rt; variadic }) ];
                Ok ())
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let templates =
    List.filter_map
      (fun item ->
        match item with
        | Ast.Func { name; const_params = _ :: _; _ } -> Some (name, item)
        | _ -> None)
      program.items
  in
  let all_strings = ref [] and funcs = ref [] and all_specs = ref [] in
  let hir_linkage = function
    | Ast.External_c -> Hir.External_c
    | Ast.Internal -> Hir.Internal
  in
  let make_context ~extra_consts ~spec_depth ~ret_ty =
    {
      structs;
      consts = (if extra_consts = [] then !consts else extra_consts @ !consts);
      arrays = !arrays;
      signatures = !sigs;
      templates;
      specializations = all_specs;
      spec_depth;
      locals = ref [];
      initialized = SS.empty;
      strings = !all_strings;
      string_ids = List.mapi (fun i value -> (value, i)) !all_strings;
      ret_ty;
      loop_depth = 0;
      in_defer = false;
      limits;
    }
  in
  let check_function_body ~name ~description ~span ~params ~ret ~stmts ~attrs ~linkage
      ~variadic ~extra_consts ~spec_depth ~require_return =
    let context = make_context ~extra_consts ~spec_depth ~ret_ty:ret in
    List.iter
      (fun (param_name, param_ty, _, _) ->
        ignore (add_local param_name param_ty context span);
        mark_init param_name context)
      params;
    let* body = check_block context stmts in
    let* () =
      if require_return && ret <> Hir.Void && not (block_must_return body) then
        error span (description ^ " `" ^ name ^ "` may reach the end without returning")
      else Ok ()
    in
    all_strings := context.strings;
    Ok
      ({ Hir.name; params; ret; body; attrs; linkage; variadic; asm_body = None }
        : Hir.func)
  in
  let add_func func = funcs := func :: !funcs in
  let check_func = function
    | Ast.Func
        { name; params; ret; body; attrs; linkage; variadic; const_params = []; span }
      ->
        let* ret = source_ty_diag span ret in
        let* params = source_params params in
        let linkage = hir_linkage linkage in
        let* func =
          match body with
          | Ast.Asm raw ->
              Ok
                ({
                   Hir.name;
                   params;
                   ret;
                   body = [];
                   attrs;
                   linkage;
                   variadic;
                   asm_body = Some raw;
                 }
                  : Hir.func)
          | Ast.Statements stmts ->
              check_function_body ~name ~description:"function" ~span ~params ~ret
                ~stmts ~attrs ~linkage ~variadic ~extra_consts:[] ~spec_depth:0
                ~require_return:(linkage <> Hir.External_c)
        in
        add_func func;
        Ok ()
    | _ -> Ok ()
  in
  let* () = Result_list.iter check_func program.items in
  let rec materialize index =
    if index >= List.length !all_specs then Ok ()
    else if index >= limits.Limits.max_specializations then
      error Span.synthetic "const specialization count limit exceeded"
    else
      let sp = List.nth !all_specs index in
      match sp.item with
      | Ast.Func
          { params; ret; body = Ast.Statements stmts; attrs; linkage; variadic; _ } ->
          let* ret = source_ty_diag Span.synthetic ret in
          let* params = source_params params in
          let* func =
            check_function_body ~name:sp.name ~description:"specialized function"
              ~span:Span.synthetic ~params ~ret ~stmts ~attrs
              ~linkage:(hir_linkage linkage) ~variadic ~extra_consts:sp.values
              ~spec_depth:(sp.depth + 1) ~require_return:true
          in
          add_func func;
          materialize (index + 1)
      | _ -> materialize (index + 1)
  in
  let* () = materialize 0 in
  let hconsts =
    List.map
      (fun (n, t, v) -> ({ Hir.name = n; ty = t; bits = v } : Hir.const_def))
      !consts
  in
  let harrays =
    List.map
      (fun (n, t, vs) -> ({ Hir.name = n; ty = t; elems = vs } : Hir.const_arr_def))
      !arrays
  in
  Ok
    ({
       Hir.structs;
       consts = hconsts;
       const_arrays = harrays;
       funcs = List.rev !funcs;
       strings = !all_strings;
     }
      : Hir.program)
