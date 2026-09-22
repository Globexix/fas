open Sema_numeric
open Sema_types

let error span message = Error [ Diag.error span message ]

let ( let* ) result continuation =
  match result with
  | Error diagnostics -> Error diagnostics
  | Ok value -> continuation value

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table
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
  | Ast.Call (Ast.Ident (name, _), args, s) -> (
      let* vals =
        Result_list.map
          (fun a ->
            const_expr ~structs ~named_types ~arrays ?resolve consts expected
              ~check_only ~validate_dead a)
          args
      in
      match (name, vals) with
      | ("shl" | "lshr" | "ashr" | "rotl" | "rotr"), [ (t, x); (count_ty, n) ] ->
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
      | "popcount", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (popcount64 x))
          else error s "builtin argument must be an integer"
      | ("ctz" | "clz"), [ (t, 0L) ] ->
          if is_int t then
            let bits = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int bits)
          else error s "builtin argument must be an integer"
      | "ctz", [ (t, x) ] ->
          if is_int t then Ok (t, Int64.of_int (trailing64 x))
          else error s "builtin argument must be an integer"
      | "clz", [ (t, x) ] ->
          if is_int t then
            let b = match t with Hir.Int q -> int_bits q | _ -> 64 in
            Ok (t, Int64.of_int (leading64 x - (64 - b)))
          else error s "builtin argument must be an integer"
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
            | Ast.And | Ast.Or -> 0L
          in
          Ok
            ( result_ty,
              List.map2
                (fun left right -> lane_mask result_element (apply left right))
                left_values right_values ))
  | Ast.Call (Ast.Ident (name, _), [ value; count ], span)
    when List.mem name [ "shl"; "lshr"; "ashr"; "rotl"; "rotr" ] ->
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
            | "shl" -> Int64.shift_left value amount
            | "lshr" -> Int64.shift_right_logical value amount
            | "ashr" -> Int64.shift_right (lane_signed element value) amount
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
