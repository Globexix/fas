open Sema_numeric
open Sema_types

let error span message = Error [ Diag.error span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let shuffle_indices_in_range lane_ty n values =
  let signed =
    match lane_ty with
    | Hir.Int (Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 | Hir.Isize) -> true
    | _ -> false
  in
  List.for_all
    (fun v ->
      let v = if signed then sign_extend_value lane_ty v else v in
      Int64.compare v 0L >= 0 && Int64.compare v (Int64.of_int (2 * n)) < 0)
    values

let ty_name = Hir.ty_name

type unresolved_shape = Unresolved_int | Unresolved_vector | Unresolved_null

let rec unresolved_shape_of expression =
  let combined left right =
    match (unresolved_shape_of left, unresolved_shape_of right) with
    | Some left_shape, Some right_shape when left_shape = right_shape -> Some left_shape
    | _ -> None
  in
  match expression with
  | Ast.Int_lit _ -> Some Unresolved_int
  | Ast.Null _ -> Some Unresolved_null
  | Ast.Unary ((Ast.Neg | Ast.Bit_not), operand, _) -> (
      match unresolved_shape_of operand with
      | Some Unresolved_int -> Some Unresolved_int
      | _ -> None)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor ),
        left,
        right,
        _ ) ->
      combined left right
  | Ast.Splat (_, _) -> Some Unresolved_vector
  | _ -> None

let rec unresolved_vector_elements expression =
  match expression with
  | Ast.Splat (element, _) -> (
      match unresolved_shape_of element with Some Unresolved_int -> true | _ -> false)
  | Ast.Binary
      ( ( Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
        | Ast.Bit_xor ),
        left,
        right,
        _ ) ->
      unresolved_vector_elements left && unresolved_vector_elements right
  | _ -> false

let operand_type_hint operation expected left right =
  match operation with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
  | Ast.Bit_xor ->
      expected
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
      match expected with
      | Some (Hir.Vec (lanes, _))
        when unresolved_vector_elements left && unresolved_vector_elements right ->
          Some (Hir.Vec (lanes, Hir.Int Hir.I32))
      | _ -> None)
  | Ast.And | Ast.Or -> None
  | Ast.Shl | Ast.Shr -> None

let sat_apply name kind x y =
  let bits = int_bits kind in
  if is_unsigned (Hir.Int kind) then
    let max_w =
      if bits >= 64 then Int64.minus_one else Int64.sub (Int64.shift_left 1L bits) 1L
    in
    match name with
    | "add_sat" ->
        let sum = Int64.add x y in
        if Int64.unsigned_compare sum x < 0 || Int64.unsigned_compare sum max_w > 0 then
          max_w
        else sum
    | _ ->
        let difference = Int64.sub x y in
        if Int64.unsigned_compare y x > 0 then 0L else difference
  else
    let max_s =
      if bits >= 64 then Int64.max_int
      else Int64.sub (Int64.shift_left 1L (bits - 1)) 1L
    in
    let min_s =
      if bits >= 64 then Int64.min_int else Int64.neg (Int64.shift_left 1L (bits - 1))
    in
    let xs = sign_extend_value (Hir.Int kind) x in
    let ys = sign_extend_value (Hir.Int kind) y in
    match name with
    | "add_sat" ->
        if Int64.compare ys 0L > 0 && Int64.compare xs (Int64.sub max_s ys) > 0 then
          max_s
        else if Int64.compare ys 0L < 0 && Int64.compare xs (Int64.sub min_s ys) < 0
        then min_s
        else Int64.add xs ys
    | _ ->
        if Int64.compare ys 0L < 0 && Int64.compare xs (Int64.add max_s ys) > 0 then
          max_s
        else if Int64.compare ys 0L > 0 && Int64.compare xs (Int64.add min_s ys) < 0
        then min_s
        else Int64.sub xs ys

let mulhi_apply kind x y =
  let bits = int_bits kind in
  if bits < 64 then
    let signed = not (is_unsigned (Hir.Int kind)) in
    let xs = if signed then sign_extend_value (Hir.Int kind) x else x in
    let ys = if signed then sign_extend_value (Hir.Int kind) y else y in
    let product = Int64.mul xs ys in
    if signed then Int64.shift_right product bits
    else Int64.shift_right_logical product bits
  else if is_unsigned (Hir.Int kind) then
    let al = Int64.logand x 0xFFFFFFFFL in
    let ah = Int64.shift_right_logical x 32 in
    let bl = Int64.logand y 0xFFFFFFFFL in
    let bh = Int64.shift_right_logical y 32 in
    let ll = Int64.mul al bl in
    let lh = Int64.mul al bh in
    let hl = Int64.mul ah bl in
    let hh = Int64.mul ah bh in
    let mid =
      Int64.add
        (Int64.add (Int64.shift_right_logical ll 32) (Int64.logand lh 0xFFFFFFFFL))
        (Int64.logand hl 0xFFFFFFFFL)
    in
    Int64.add hh
      (Int64.add
         (Int64.shift_right_logical lh 32)
         (Int64.add
            (Int64.shift_right_logical hl 32)
            (Int64.shift_right_logical mid 32)))
  else
    let x0 = Int64.logand x 0xFFFFFFFFL in
    let x1 = Int64.shift_right x 32 in
    let y0 = Int64.logand y 0xFFFFFFFFL in
    let y1 = Int64.shift_right y 32 in
    let z0 = Int64.mul x0 y0 in
    let t = Int64.add (Int64.mul x1 y0) (Int64.shift_right_logical z0 32) in
    let z1 = Int64.add (Int64.logand t 0xFFFFFFFFL) (Int64.mul x0 y1) in
    Int64.add (Int64.mul x1 y1)
      (Int64.add (Int64.shift_right t 32) (Int64.shift_right z1 32))

let sat_or_mulhi name kind x y =
  if name = "mul_hi" then mulhi_apply kind x y else sat_apply name kind x y

let rec const_expr ?(structs = []) ?(named_types = []) ?(arrays = []) ?resolve consts
    expected ?(check_only = false) ?(validate_dead = true) = function
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
      | None -> (
          match resolve with
          | Some resolve -> resolve ~check_only n s
          | None -> error s "constant expression requires a known constant"))
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
      let* t, v =
        const_expr ~structs ~named_types ~arrays ?resolve consts expected ~check_only
          ~validate_dead e
      in
      if not (is_int t) then error s "unary minus requires an integer"
      else Ok (t, mask_value t (Int64.neg v))
  | Ast.Unary (Ast.Bit_not, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~arrays ?resolve consts expected ~check_only
          ~validate_dead e
      in
      if not (is_int t) then error s "bitwise not requires an integer"
      else Ok (t, mask_value t (Int64.lognot v))
  | Ast.Unary (Ast.Not, e, s) ->
      let* t, v =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
          ~validate_dead e
      in
      if t <> Hir.Bool then error s "logical not requires bool"
      else Ok (Hir.Bool, if v = 0L then 1L else 0L)
  | Ast.Binary (((Ast.And | Ast.Or) as op), l, r, s) ->
      let* lt, lv =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
          ~validate_dead l
      in
      if lt <> Hir.Bool then error s "logical operands must be bool"
      else if
        (not check_only) && ((op = Ast.And && lv = 0L) || (op = Ast.Or && lv <> 0L))
      then
        let* _ =
          const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only:true
            ~validate_dead r
        in
        Ok (Hir.Bool, if op = Ast.And then 0L else 1L)
      else
        let* rt, rv =
          const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
            ~validate_dead r
        in
        if rt <> Hir.Bool then error s "logical operands must be bool"
        else Ok (Hir.Bool, if rv <> 0L then 1L else 0L)
  | Ast.Binary (((Ast.Shl | Ast.Shr) as op), l, r, s) ->
      let* lt, lv =
        const_expr ~structs ~named_types ~arrays ?resolve consts
          (match expected with Some (Hir.Int _) -> expected | _ -> None)
          ~check_only ~validate_dead l
      in
      let* rt, rv =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
          ~validate_dead r
      in
      let* () =
        match lt with
        | Hir.Int _ -> Ok ()
        | _ -> error s "shift value must be an integer or integer vector"
      in
      let* () =
        match rt with
        | Hir.Int _ -> Ok ()
        | Hir.Vec (_, Hir.Bool) -> error s "shift count must be an integer"
        | Hir.Vec _ -> error s "shift count must be a scalar integer for a scalar value"
        | _ -> error s "shift count must be an integer"
      in
      let bits = match lt with Hir.Int k -> int_bits k | _ -> 64 in
      let k = Int64.to_int (Int64.logand rv (Int64.of_int (bits - 1))) in
      let v =
        if k = 0 then lv
        else
          match op with
          | Ast.Shl -> Int64.shift_left lv k
          | _ ->
              if is_unsigned lt then Int64.shift_right_logical lv k
              else Int64.shift_right (sign_extend_bits lt lv) k
      in
      Ok (lt, mask_value lt v)
  | Ast.Binary (op, l, r, s) ->
      let* (lt, lv), (rt, rv) =
        let hint = operand_type_hint op expected l r in
        match (unresolved_shape_of l, unresolved_shape_of r) with
        | Some _, None ->
            let* rt, rv =
              const_expr ~structs ~named_types ~arrays ?resolve consts hint ~check_only
                ~validate_dead r
            in
            let* lt, lv =
              const_expr ~structs ~named_types ~arrays ?resolve consts (Some rt)
                ~check_only ~validate_dead l
            in
            Ok ((lt, lv), (rt, rv))
        | _ ->
            let* lt, lv =
              const_expr ~structs ~named_types ~arrays ?resolve consts hint ~check_only
                ~validate_dead l
            in
            let* rt, rv =
              const_expr ~structs ~named_types ~arrays ?resolve consts (Some lt)
                ~check_only ~validate_dead r
            in
            Ok ((lt, lv), (rt, rv))
      in
      let* result_ty =
        binary_result_type ~mismatch:"constant operands have different types" s op lt rt
      in
      if (not check_only) && (op = Ast.Div || op = Ast.Rem) && rv = 0L then
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
          (not check_only) && op = Ast.Div
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
                if check_only && rv = 0L then 0L
                else if
                  check_only
                  && (not (is_unsigned lt))
                  && signed_lv = signed_min && signed_rv = Int64.minus_one
                then 0L
                else if is_unsigned lt then Int64.unsigned_div lv rv
                else Int64.div signed_lv signed_rv
            | Rem ->
                if check_only && rv = 0L then 0L
                else if is_unsigned lt then Int64.unsigned_rem lv rv
                else Int64.rem signed_lv signed_rv
            | Eq -> if lv = rv then 1L else 0L
            | Ne -> if lv <> rv then 1L else 0L
            | Lt -> if cmp < 0 then 1L else 0L
            | Le -> if cmp <= 0 then 1L else 0L
            | Gt -> if cmp > 0 then 1L else 0L
            | Ge -> if cmp >= 0 then 1L else 0L
            | Ast.Shl | Ast.Shr ->
                let bits = match lt with Hir.Int q -> int_bits q | _ -> 64 in
                let k = Int64.to_int (Int64.logand rv (Int64.of_int (bits - 1))) in
                if k = 0 then lv
                else if op = Ast.Shl then Int64.shift_left lv k
                else if is_unsigned lt then Int64.shift_right_logical lv k
                else Int64.shift_right (sign_extend_bits lt lv) k
            | And -> if lv <> 0L && rv <> 0L then 1L else 0L
            | Or -> if lv <> 0L || rv <> 0L then 1L else 0L
          in
          Ok (result_ty, mask_value result_ty result)
  | Ast.Ternary (c, a, b, s) ->
      let* ct, cv =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
          ~validate_dead c
      in
      if ct <> Hir.Bool then error s "ternary condition must be bool"
      else if cv <> 0L then
        let* at, av =
          const_expr ~structs ~named_types ~arrays ?resolve consts expected ~check_only
            ~validate_dead a
        in
        let* () =
          if not validate_dead then Ok ()
          else
            let* bt, _ =
              const_expr ~structs ~named_types ~arrays ?resolve consts (Some at)
                ~check_only:true ~validate_dead b
            in
            ensure_expected bt at (Ast.expr_span b)
        in
        Ok (at, av)
      else
        let* bt, bv =
          const_expr ~structs ~named_types ~arrays ?resolve consts expected ~check_only
            ~validate_dead b
        in
        let* () =
          if not validate_dead then Ok ()
          else
            let* at, _ =
              const_expr ~structs ~named_types ~arrays ?resolve consts (Some bt)
                ~check_only:true ~validate_dead a
            in
            ensure_expected at bt (Ast.expr_span a)
        in
        Ok (bt, bv)
  | Ast.Cast (k, dst, e, s) ->
      let* dt = source_ty_with_values named_types consts s dst in
      let scalar_source () =
        const_expr ~structs ~named_types ~arrays ?resolve consts ~check_only
          ~validate_dead
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
      let* st, v, reshaped =
        match scalar_source () with
        | Ok (st, v) -> Ok (st, v, false)
        | Error scalar_error -> (
            match
              vector_const_expr ~structs ~named_types ~arrays ?resolve consts None e
            with
            | Ok (st, values) when k = Ast.Bitcast && cast_legal k st dt -> (
                match constant_bitcast st values dt with
                | Ok [ value ] -> Ok (st, value, true)
                | _ -> Error scalar_error)
            | _ -> Error scalar_error)
      in
      let* () =
        if cast_legal k st dt then Ok ()
        else error s "illegal cast for source and destination widths"
      in
      let* () =
        if ((st = Hir.Bool || is_int st) && (dt = Hir.Bool || is_int dt)) || reshaped
        then Ok ()
        else error s "constant cast requires scalar integer or bool types"
      in
      let sb =
        match st with
        | Hir.Bool -> 1
        | Hir.Int q -> int_bits q
        | Hir.Ptr _ | Hir.ConstPtr _ -> Target_layout.current.pointer_size * 8
        | _ -> 0
      in
      let result =
        if reshaped then v
        else
          match k with
          | Ast.Sext when sb < 64 ->
              let shift = 64 - sb in
              Int64.shift_right (Int64.shift_left v shift) shift
          | Ast.Trunc when dt = Hir.Bool -> Int64.logand v 1L
          | _ -> v
      in
      Ok (dt, mask_value dt result)
  | Ast.Call (Ast.Ident ("len", _), [ Ast.String_lit (cstr, v, _) ], s) ->
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else Ok (Hir.Int Hir.Usize, Int64.of_int (String.length v))
  | Ast.Call (Ast.Ident ("len", _), [ Ast.Ident (name, _) ], s) -> (
      match lookup name arrays with
      | Some (_, Hir.Array (n, _), _) -> Ok (Hir.Int Hir.Usize, Int64.of_int n)
      | _ -> error s "len requires a fixed array or string literal")
  | Ast.Call (Ast.Ident (name, _), [ arg ], _s) when name = "any" || name = "all" -> (
      match
        vector_const_expr ~structs ~named_types ~arrays ?resolve consts None arg
      with
      | Ok (Hir.Vec (_, Hir.Bool), values) ->
          let result =
            if name = "any" then List.exists (fun v -> v <> 0L) values
            else List.for_all (fun v -> v <> 0L) values
          in
          Ok (Hir.Bool, if result then 1L else 0L)
      | Ok _ -> error (Ast.expr_span arg) "builtin argument must be a bool vector"
      | Error e -> Error e)
  | Ast.Call (Ast.Ident (name, _), [ arg ], _s)
    when name = "reduce_sum" || name = "reduce_min" || name = "reduce_max"
         || name = "reduce_and" || name = "reduce_or" || name = "reduce_xor" -> (
      match
        vector_const_expr ~structs ~named_types ~arrays ?resolve consts None arg
      with
      | Ok (Hir.Vec (_, Hir.Int kind), (_ :: _ as values)) ->
          let ty = Hir.Int kind in
          let signed =
            match kind with
            | Hir.I8 | Hir.I16 | Hir.I32 | Hir.I64 | Hir.Isize -> true
            | _ -> false
          in
          let result =
            match name with
            | "reduce_sum" ->
                List.fold_left (fun acc v -> mask_value ty (Int64.add acc v)) 0L values
            | "reduce_and" ->
                List.fold_left Int64.logand (mask_value ty (-1L)) values
                |> mask_value ty
            | "reduce_or" -> List.fold_left Int64.logor 0L values |> mask_value ty
            | "reduce_xor" -> List.fold_left Int64.logxor 0L values |> mask_value ty
            | "reduce_min" | "reduce_max" ->
                let better a b =
                  let c =
                    if signed then
                      Int64.compare (sign_extend_value ty a) (sign_extend_value ty b)
                    else Int64.unsigned_compare a b
                  in
                  if name = "reduce_min" then c <= 0 else c >= 0
                in
                List.fold_left
                  (fun acc v -> if better acc v then acc else v)
                  (List.hd values) values
            | _ -> 0L
          in
          Ok (ty, result)
      | Ok _ -> error (Ast.expr_span arg) "reduction argument must be an integer vector"
      | Error e -> Error e)
  | Ast.Call (Ast.Ident (name, _), args, s) -> (
      let* vals =
        Result_list.map
          (fun a ->
            const_expr ~structs ~named_types ~arrays ?resolve consts expected
              ~check_only ~validate_dead a)
          args
      in
      match (name, vals) with
      | ("rotl" | "rotr"), [ (t, x); (count_ty, n) ] ->
          let* () =
            if is_int t && is_int count_ty then Ok ()
            else error s "builtin shift arguments must be integers"
          in
          let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
          let k = Int64.to_int (Int64.logand n (Int64.of_int (bits - 1))) in
          let v =
            if k = 0 then x
            else
              match name with
              | "rotl" ->
                  Int64.logor (Int64.shift_left x k)
                    (Int64.shift_right_logical x (bits - k))
              | _ ->
                  Int64.logor
                    (Int64.shift_right_logical x k)
                    (Int64.shift_left x (bits - k))
          in
          Ok (t, mask_value t v)
      | "popcount", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (popcount64 x))
          else error s "builtin argument must be an integer or an integer vector"
      | ("ctz" | "clz"), [ (t, 0L) ] ->
          if is_int t then
            let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int bits)
          else error s "builtin argument must be an integer or an integer vector"
      | "ctz", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (trailing64 x))
          else error s "builtin argument must be an integer or an integer vector"
      | "clz", [ (t, x) ] ->
          if is_int t then
            let b = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int (leading64 x - (64 - b)))
          else error s "builtin argument must be an integer or an integer vector"
      | ("add_sat" | "sub_sat" | "mul_hi"), [ (t, x); (t2, y) ] -> (
          match t with
          | Hir.Int kind when t = t2 -> Ok (t, mask_value t (sat_or_mulhi name kind x y))
          | Hir.Int _ -> error s "builtin arguments must have the same type"
          | _ -> error s "builtin arguments must be integers or integer vectors")
      | _ -> error s "invalid constant builtin call")
  | Ast.Sizeof (t, s) ->
      let* t = source_ty_with_values named_types consts s t in
      let* n, _ = layout_diag s structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Alignof (t, s) ->
      let* t = source_ty_with_values named_types consts s t in
      let* _, n = layout_diag s structs t in
      Ok (Hir.Int Hir.Usize, Int64.of_int n)
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_with_values named_types consts s t in
      match t with
      | Hir.Struct sn -> (
          match field_info structs sn n with
          | Some f -> Ok (Hir.Int Hir.Usize, Int64.of_int f.offset)
          | None -> error s "unknown field in offsetof")
      | _ -> error s "offsetof requires a struct")
  | expr -> error (Ast.expr_span expr) "expression is not compile-time constant"

and vector_const_expr ?(structs = []) ?(named_types = []) ?(arrays = []) ?resolve consts
    expected ?(check_only = false) expression =
  let lane_type = function Hir.Vec (_, element) -> Some element | _ -> None in
  let lane_mask ty value = mask_value ty value in
  let lane_signed ty value = sign_extend_value ty value in
  let evaluate =
    vector_const_expr ~structs ~named_types ~arrays ?resolve consts ~check_only
  in
  match expression with
  | Ast.Ident (name, span) -> (
      match lookup name arrays with
      | Some (_, (Hir.Vec _ as ty), values) -> Ok (ty, values)
      | _ -> error span "constant expression requires a known vector constant")
  | Ast.Splat (value, span) -> (
      match expected with
      | Some (Hir.Vec (lanes, element) as ty) ->
          let* actual, value =
            const_expr ~structs ~named_types ~arrays ?resolve consts (Some element)
              ~check_only value
          in
          let* () = ensure_expected actual element (Ast.expr_span expression) in
          Ok (ty, List.init lanes (fun _ -> lane_mask element value))
      | _ -> error span "splat requires a vector type context")
  | Ast.Unary (Ast.Not, value, span) -> (
      let* ty, values = evaluate expected value in
      match ty with
      | Hir.Vec (_, Hir.Bool) ->
          Ok (ty, List.map (fun value -> if value = 0L then 1L else 0L) values)
      | _ -> error span "logical not requires a bool vector")
  | Ast.Binary (((Ast.Shl | Ast.Shr) as op), value, count, span) ->
      let* ty, values = evaluate expected value in
      let* element =
        match lane_type ty with
        | Some (Hir.Int _ as element) -> Ok element
        | _ -> error span "shift value must be an integer or integer vector"
      in
      let* count_ty, counts =
        match evaluate None count with
        | Ok (count_ty, counts) -> Ok (count_ty, counts)
        | Error _ -> (
            match
              const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
                count
            with
            | Ok (count_ty, count_value) -> Ok (count_ty, [ count_value ])
            | Error diagnostics -> Error diagnostics)
      in
      let* per_lane =
        match count_ty with
        | Hir.Int _ -> Ok (List.map (fun _ -> List.hd counts) values)
        | Hir.Vec (count_lanes, Hir.Int _) ->
            if count_lanes = List.length values then Ok counts
            else error span "shift count lanes must match the value lanes"
        | Hir.Vec (_, Hir.Bool) -> error span "shift count must be an integer"
        | _ -> error span "shift count must be an integer"
      in
      let bits = Option.get (integer_value_bit_width element) in
      let apply amount value =
        let amount = Int64.to_int (Int64.logand amount (Int64.of_int (bits - 1))) in
        if amount = 0 then value
        else
          match op with
          | Ast.Shl -> Int64.shift_left value amount
          | _ ->
              if is_unsigned element then Int64.shift_right_logical value amount
              else Int64.shift_right (lane_signed element value) amount
      in
      Ok (ty, List.map2 (fun c v -> lane_mask element (apply c v)) per_lane values)
  | Ast.Binary (operation, left, right, span) -> (
      let hint = operand_type_hint operation expected left right in
      let* left_ty, left_values, right_ty, right_values =
        match (unresolved_shape_of left, unresolved_shape_of right) with
        | Some _, None ->
            let* right_ty, right_values = evaluate hint right in
            let* left_ty, left_values = evaluate (Some right_ty) left in
            Ok (left_ty, left_values, right_ty, right_values)
        | _ ->
            let* left_ty, left_values = evaluate hint left in
            let* right_ty, right_values = evaluate (Some left_ty) right in
            Ok (left_ty, left_values, right_ty, right_values)
      in
      let* result_ty =
        binary_result_type ~mismatch:"constant operands have different types" span
          operation left_ty right_ty
      in
      let* element =
        match lane_type left_ty with
        | Some element -> Ok element
        | None -> error span "vector operation requires vector operands"
      in
      let signed_min =
        match element with
        | Hir.Int kind ->
            let bits = int_bits kind in
            if bits = 64 then Int64.min_int
            else Int64.neg (Int64.shift_left 1L (bits - 1))
        | _ -> 0L
      in
      let rec first_offense lefts rights =
        match (lefts, rights) with
        | left :: left_rest, right :: right_rest ->
            if (operation = Ast.Div || operation = Ast.Rem) && right = 0L then
              Some "division by zero in constant expression"
            else if
              operation = Ast.Div
              && (not (is_unsigned element))
              && lane_signed element left = signed_min
              && lane_signed element right = Int64.minus_one
            then Some "signed division overflow in constant expression"
            else first_offense left_rest right_rest
        | _ -> None
      in
      let offense =
        if check_only then None else first_offense left_values right_values
      in
      match offense with
      | Some message -> error span message
      | None ->
          let result_element =
            match operation with
            | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Hir.Bool
            | _ -> element
          in
          let apply left right =
            let signed_left = lane_signed element left in
            let signed_right = lane_signed element right in
            let compare =
              if is_unsigned element then Int64.unsigned_compare left right
              else Int64.compare signed_left signed_right
            in
            match operation with
            | Ast.Add -> Int64.add left right
            | Ast.Sub -> Int64.sub left right
            | Ast.Mul -> Int64.mul left right
            | Ast.Bit_and -> Int64.logand left right
            | Ast.Bit_or -> Int64.logor left right
            | Ast.Bit_xor -> Int64.logxor left right
            | Ast.Div ->
                if check_only && right = 0L then 0L
                else if
                  check_only
                  && (not (is_unsigned element))
                  && signed_left = signed_min
                  && signed_right = Int64.minus_one
                then 0L
                else if is_unsigned element then Int64.unsigned_div left right
                else Int64.div signed_left signed_right
            | Ast.Rem ->
                if check_only && right = 0L then 0L
                else if is_unsigned element then Int64.unsigned_rem left right
                else Int64.rem signed_left signed_right
            | Ast.Eq -> if left = right then 1L else 0L
            | Ast.Ne -> if left <> right then 1L else 0L
            | Ast.Lt -> if compare < 0 then 1L else 0L
            | Ast.Le -> if compare <= 0 then 1L else 0L
            | Ast.Gt -> if compare > 0 then 1L else 0L
            | Ast.Ge -> if compare >= 0 then 1L else 0L
            | Ast.Shl | Ast.Shr ->
                let bits = Option.get (integer_value_bit_width element) in
                let amount =
                  Int64.to_int (Int64.logand right (Int64.of_int (bits - 1)))
                in
                if amount = 0 then left
                else if operation = Ast.Shl then Int64.shift_left left amount
                else if is_unsigned element then Int64.shift_right_logical left amount
                else Int64.shift_right (lane_signed element left) amount
            | Ast.And | Ast.Or -> 0L
          in
          Ok
            ( result_ty,
              List.map2
                (fun left right -> lane_mask result_element (apply left right))
                left_values right_values ))
  | Ast.Call (Ast.Ident (name, _), [ value ], span)
    when List.mem name [ "popcount"; "clz"; "ctz" ] ->
      let* ty, values = evaluate expected value in
      let* kind =
        match lane_type ty with
        | Some (Hir.Int k) -> Ok k
        | _ -> error span "builtin argument must be an integer or an integer vector"
      in
      let apply value =
        match name with
        | "popcount" -> Int64.of_int (popcount64 value)
        | "ctz" ->
            if value = 0L then Int64.of_int (int_bits kind)
            else Int64.of_int (trailing64 value)
        | _ ->
            let bits = int_bits kind in
            if value = 0L then Int64.of_int bits
            else Int64.of_int (leading64 value - (64 - bits))
      in
      Ok (ty, List.map (fun value -> lane_mask (Hir.Int kind) (apply value)) values)
  | Ast.Call (Ast.Ident (name, _), [ value; count ], span)
    when List.mem name [ "rotl"; "rotr" ] ->
      let* ty, values = evaluate expected value in
      let* element =
        match lane_type ty with
        | Some (Hir.Int _ as element) -> Ok element
        | _ -> error span "builtin shift value must be an integer vector"
      in
      let* count_ty, count =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only count
      in
      if not (is_int count_ty) then error span "builtin shift count must be an integer"
      else
        let bits = Option.get (integer_value_bit_width element) in
        let amount = Int64.to_int (Int64.logand count (Int64.of_int (bits - 1))) in
        let apply value =
          if amount = 0 then value
          else
            match name with
            | "rotl" ->
                Int64.logor
                  (Int64.shift_left value amount)
                  (Int64.shift_right_logical value (bits - amount))
            | "rotr" ->
                Int64.logor
                  (Int64.shift_right_logical value amount)
                  (Int64.shift_left value (bits - amount))
            | _ -> value
        in
        Ok (ty, List.map (fun value -> lane_mask element (apply value)) values)
  | Ast.Call (Ast.Ident (name, _), [ a; idx ], span) when name = "permute" ->
      let* at, avalues = evaluate expected a in
      let* n, element =
        match at with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool) as e)) -> Ok (n, e)
        | _ -> error span "permute value must be a vector"
      in
      let* idx_ty, idx_values = evaluate None idx in
      let* () =
        match idx_ty with
        | Hir.Vec (_, Hir.Int (Hir.U8 | Hir.U16 | Hir.U32 | Hir.U64 | Hir.Usize)) ->
            Ok ()
        | _ -> error span "permute indices must be an unsigned integer vector"
      in
      let source = Array.of_list avalues in
      Ok
        ( Hir.Vec (List.length idx_values, element),
          List.map
            (fun v ->
              if Int64.unsigned_compare v (Int64.of_int n) < 0 then
                lane_mask element source.(Int64.to_int v)
              else 0L)
            idx_values )
  | Ast.Call (Ast.Ident (name, _), [ a; b; sel ], span) when name = "shuffle" ->
      let* at, avalues = evaluate expected a in
      let* bt, bvalues = evaluate (Some at) b in
      let* () =
        if at = bt then Ok ()
        else error span "builtin arguments must have the same type"
      in
      let* n, element =
        match at with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool) as e)) -> Ok (n, e)
        | _ -> error span "shuffle operands must be vectors"
      in
      let* sel_ty, sel_values = evaluate None sel in
      let* sel_elem =
        match sel_ty with
        | Hir.Vec (_, (Hir.Int _ as e)) -> Ok e
        | _ ->
            error span "shuffle indices must be a compile-time constant integer vector"
      in
      let* () =
        if shuffle_indices_in_range sel_elem n sel_values then Ok ()
        else error span "shuffle index out of range"
      in
      let source = Array.of_list (avalues @ bvalues) in
      Ok
        ( Hir.Vec (List.length sel_values, element),
          List.map (fun v -> lane_mask element source.(Int64.to_int v)) sel_values )
  | Ast.Call (Ast.Ident (name, _), [ m; y; z ], span) when name = "select" ->
      let* mask_ty, mask_values = evaluate None m in
      let* yes_ty, yes_values = evaluate expected y in
      let* no_ty, no_values = evaluate (Some yes_ty) z in
      let* lanes =
        match mask_ty with
        | Hir.Vec (n, Hir.Bool) -> Ok n
        | _ -> error span "select mask must be a bool vector"
      in
      let* () =
        if yes_ty = no_ty then Ok ()
        else error span "builtin arguments must have the same type"
      in
      let* element =
        match yes_ty with
        | Hir.Vec (n, ((Hir.Int _ | Hir.Bool) as e)) when n = lanes -> Ok e
        | _ -> error span "select values must be vectors with the mask lane count"
      in
      Ok
        ( yes_ty,
          List.map2
            (fun mv (yv, nv) -> lane_mask element (if mv <> 0L then yv else nv))
            mask_values
            (List.combine yes_values no_values) )
  | Ast.Call (Ast.Ident (name, _), [ left; right ], span)
    when List.mem name [ "add_sat"; "sub_sat"; "mul_hi" ] ->
      let* left_ty, left_values, right_ty, right_values =
        match (unresolved_shape_of left, unresolved_shape_of right) with
        | Some _, None ->
            let* right_ty, right_values = evaluate expected right in
            let* left_ty, left_values = evaluate (Some right_ty) left in
            Ok (left_ty, left_values, right_ty, right_values)
        | _ ->
            let* left_ty, left_values = evaluate expected left in
            let* right_ty, right_values = evaluate (Some left_ty) right in
            Ok (left_ty, left_values, right_ty, right_values)
      in
      let* () =
        if left_ty = right_ty then Ok ()
        else error span "builtin arguments must have the same type"
      in
      let* kind =
        match lane_type left_ty with
        | Some (Hir.Int k) -> Ok k
        | _ -> error span "builtin arguments must be integers or integer vectors"
      in
      Ok
        ( left_ty,
          List.map2
            (fun a b -> lane_mask (Hir.Int kind) (sat_or_mulhi name kind a b))
            left_values right_values )
  | Ast.Ternary (condition, yes, no, span) ->
      let* condition_ty, condition_value =
        const_expr ~structs ~named_types ~arrays ?resolve consts None ~check_only
          condition
      in
      if condition_ty <> Hir.Bool then error span "ternary condition must be bool"
      else if condition_value <> 0L then
        let* yes_ty, yes_values = evaluate expected yes in
        let* no_ty, _ =
          vector_const_expr ~structs ~named_types ~arrays ?resolve consts (Some yes_ty)
            ~check_only:true no
        in
        let* () = ensure_expected no_ty yes_ty (Ast.expr_span no) in
        Ok (yes_ty, yes_values)
      else
        let* no_ty, no_values = evaluate expected no in
        let* yes_ty, _ =
          vector_const_expr ~structs ~named_types ~arrays ?resolve consts (Some no_ty)
            ~check_only:true yes
        in
        let* () = ensure_expected yes_ty no_ty (Ast.expr_span yes) in
        Ok (no_ty, no_values)
  | Ast.Cast (kind, destination, value, span) ->
      let* destination = source_ty_with_values named_types consts span destination in
      let* source, values =
        match evaluate None value with
        | Ok result -> Ok result
        | Error _ ->
            let context =
              if
                kind = Ast.Bitcast
                &&
                match value with
                | Ast.Int_lit _ | Ast.Unary (Ast.Neg, Ast.Int_lit _, _) -> true
                | _ -> false
              then Some destination
              else None
            in
            let* source, value =
              const_expr ~structs ~named_types ~arrays ?resolve consts context
                ~check_only value
            in
            Ok (source, [ value ])
      in
      let* () =
        if cast_legal kind source destination then Ok ()
        else error span "illegal cast for source and destination widths"
      in
      if kind = Ast.Bitcast then
        let* values =
          match constant_bitcast source values destination with
          | Ok values -> Ok values
          | Error () -> error span "illegal constant bitcast representation"
        in
        Ok (destination, values)
      else
        let* source_element, destination_element =
          match (source, destination) with
          | ( Hir.Vec (source_lanes, source_element),
              Hir.Vec (destination_lanes, destination_element) )
            when source_lanes = destination_lanes ->
              Ok (source_element, destination_element)
          | _ -> error span "constant vector cast requires matching lane counts"
        in
        let source_width = Option.get (integer_value_bit_width source_element) in
        let convert value =
          let value =
            match kind with
            | Ast.Sext when source_width < 64 ->
                let shift = 64 - source_width in
                Int64.shift_right (Int64.shift_left value shift) shift
            | Ast.Trunc when destination_element = Hir.Bool -> Int64.logand value 1L
            | Ast.Zext | Ast.Sext | Ast.Trunc | Ast.Bitcast -> value
          in
          lane_mask destination_element value
        in
        Ok (destination, List.map convert values)
  | _ ->
      error (Ast.expr_span expression)
        "expression is not a compile-time vector constant"

let resolve_scalar_declarations ~structs ~named_types ~resolve_type ~strict items =
  let declarations =
    List.filter_map
      (function
        | Ast.Const { name; ty; value; span } -> Some (name, (ty, value, span))
        | _ -> None)
      items
  in
  let declaration_table = Hashtbl.create (List.length declarations) in
  List.iter
    (fun (name, declaration) -> Hashtbl.replace declaration_table name declaration)
    declarations;
  let values = Hashtbl.create (List.length declarations) in
  let visiting = Hashtbl.create (List.length declarations) in
  let requires_non_scalar = function
    | [ diagnostic ] ->
        diagnostic.Diag.message = "constant expression requires a known scalar constant"
        || diagnostic.Diag.message
           = "constant expression requires a known vector constant"
    | _ -> false
  in
  let rec resolve ~check_only name span =
    match Hashtbl.find_opt values name with
    | Some (ty, value) -> Ok (ty, value)
    | None -> (
        match Hashtbl.find_opt declaration_table name with
        | None -> error span "constant expression requires a known constant"
        | Some (source_type, initial_value, declaration_span) -> (
            let* ty = resolve_type declaration_span source_type in
            if ty <> Hir.Bool && not (is_int ty) then
              error span "constant expression requires a known scalar constant"
            else if check_only then Ok (ty, 0L)
            else if Hashtbl.mem visiting name then
              error span
                (Printf.sprintf "cyclic constant dependency involving `%s`" name)
            else
              let () = Hashtbl.add visiting name () in
              let result =
                let* actual_ty, value =
                  const_expr ~structs ~named_types ~resolve [] (Some ty) initial_value
                in
                if Hir.ty_equal actual_ty ty then Ok (ty, value)
                else error declaration_span "constant initializer type mismatch"
              in
              Hashtbl.remove visiting name;
              match result with
              | Error _ as failure -> failure
              | Ok (ty, value) as resolved ->
                  Hashtbl.replace values name (ty, value);
                  resolved))
  in
  let rec collect = function
    | [] ->
        Ok
          (List.filter_map
             (fun (name, _) ->
               match Hashtbl.find_opt values name with
               | Some (ty, value) -> Some (name, ty, value)
               | None -> None)
             declarations)
    | (name, (source_type, _, span)) :: rest -> (
        match resolve_type span source_type with
        | Error _ -> collect rest
        | Ok ty when ty <> Hir.Bool && not (is_int ty) -> collect rest
        | Ok _ -> (
            match resolve ~check_only:false name span with
            | Ok _ -> collect rest
            | Error diagnostics when requires_non_scalar diagnostics -> collect rest
            | Error _ when not strict -> collect rest
            | Error _ as failure -> failure))
  in
  collect declarations
