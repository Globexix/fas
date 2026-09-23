open Sema_constants
open Sema_flow
open Sema_numeric
open Sema_specialization
open Sema_types
open Sema_context
module String_set = Set.Make (String)
module String_map = Map.Make (String)

let error span message = Error [ Diag.error span message ]
let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

let phase_invariant = function
  | Ok () -> Ok ()
  | Error message -> error Span.synthetic ("internal error: " ^ message)

let validate_generic_params named_types params =
  let* () =
    Result_list.iter
      (function
        | Ast.Type_param { name; span } -> validate_binding_name span name
        | Ast.Const_param cp ->
            let* () = validate_binding_name cp.span cp.name in
            let* t = source_ty_diag named_types cp.span cp.ty in
            if t = Hir.Bool || is_int t then Ok ()
            else error cp.span "const parameter type must be a scalar integer or bool")
      params
  in
  let rec dup seen = function
    | [] -> Ok ()
    | Ast.Type_param { name; span } :: rest | Ast.Const_param { name; span; _ } :: rest
      ->
        if List.mem name seen then
          error span (Printf.sprintf "duplicate generic parameter `%s`" name)
        else dup (name :: seen) rest
  in
  dup [] params

let validate_function_params generic_params params =
  let generic_names =
    List.map
      (function Ast.Type_param { name; _ } | Ast.Const_param { name; _ } -> name)
      generic_params
  in
  let rec validate seen = function
    | [] -> Ok ()
    | (parameter : Ast.param) :: rest ->
        let* () = validate_binding_name parameter.span parameter.name in
        if List.mem parameter.name generic_names then
          error parameter.span
            (Printf.sprintf "parameter `%s` conflicts with a generic parameter"
               parameter.name)
        else if List.mem parameter.name seen then
          error parameter.span
            (Printf.sprintf "duplicate parameter `%s`" parameter.name)
        else validate (parameter.name :: seen) rest
  in
  validate [] params

let ty_name = Hir.ty_name

let extern_c_value_type = function
  | Hir.Bool | Hir.Int _ | Hir.Ptr _ | Hir.ConstPtr _ -> true
  | Hir.Void | Hir.Array _ | Hir.Vec _ | Hir.Struct _ | Hir.Opaque _ -> false

let validate_extern_c_signature span params converted ret =
  let rec validate_params params converted =
    match (params, converted) with
    | [], [] -> Ok ()
    | (param : Ast.param) :: param_rest, (_, ty) :: converted_rest ->
        if extern_c_value_type ty then validate_params param_rest converted_rest
        else
          error param.span
            (Printf.sprintf
               "extern \"C\" parameter `%s` cannot use `%s` by value; use a pointer"
               param.name (Hir.ty_name ty))
    | _ -> error span "internal error: extern parameter list mismatch"
  in
  let* () = validate_params params converted in
  if ret = Hir.Void || extern_c_value_type ret then Ok ()
  else
    error span
      (Printf.sprintf "extern \"C\" cannot return `%s` by value; use an output pointer"
         (Hir.ty_name ret))

let rec source_ty_in_context c span = function
  | Ast.Named_type name when Option.is_some (lookup_local name c) ->
      error span (Printf.sprintf "`%s` is a value, not a type" name)
  | Ast.Named_type name -> (
      match lookup_top_level name c.top_level_bindings with
      | Some { declaration_kind = Top_const; _ } ->
          error span (Printf.sprintf "`%s` is a constant, not a type" name)
      | Some { declaration_kind = Top_function; _ } ->
          error span (Printf.sprintf "`%s` is a function, not a type" name)
      | Some { declaration_kind = Top_type; _ } | None ->
          source_ty_diag c.named_types span (Ast.Named_type name))
  | Ast.Ptr ty ->
      let* ty = source_ty_in_context c span ty in
      Ok (Hir.Ptr ty)
  | Ast.Ptr_const ty ->
      let* ty = source_ty_in_context c span ty in
      Ok (Hir.ConstPtr ty)
  | Ast.Array (length, ty) ->
      source_aggregate_in_context c span (fun n t -> Hir.Array (n, t)) length ty
  | Ast.Vec (length, ty) ->
      source_aggregate_in_context c span (fun n t -> Hir.Vec (n, t)) length ty
  | ty -> source_ty_diag c.named_types span ty

and source_aggregate_in_context c span make length element =
  let* length =
    match int_of_string_opt length with
    | Some _ -> Ok length
    | None when Option.is_some (lookup_local length c) ->
        error span (Printf.sprintf "`%s` is not a compile-time constant" length)
    | None -> resolve_aggregate_length c.consts span length
  in
  let* element = source_ty_in_context c span element in
  match int_of_string_opt length with
  | Some length when length < 0 -> error span "negative aggregate length"
  | Some length -> Ok (make length element)
  | None -> error span "aggregate length is not a machine integer"

let intern_string c span s =
  let pool = c.string_pool in
  match Hashtbl.find_opt pool.index s with
  | Some id -> Ok id
  | None ->
      let size = String.length s in
      let budget = pool.budget in
      if size > budget then
        error span
          (Printf.sprintf
             "string literal bytes exceed budget max_interned_string_bytes of %d \
              (profile %s)"
             budget pool.budget_profile)
      else if pool.bytes_used > budget - size then
        error span
          (Printf.sprintf
             "cumulative interned string bytes exceed budget max_interned_string_bytes \
              of %d (profile %s)"
             budget pool.budget_profile)
      else
        let id = pool.next_id in
        pool.next_id <- id + 1;
        pool.reversed <- s :: pool.reversed;
        pool.bytes_used <- pool.bytes_used + size;
        Hashtbl.add pool.index s id;
        Ok id

let with_dead_check c dead check = Sema_flow.with_dead_check c.flow dead check

let rec rooted_in_constant = function
  | Hir.Const_array _ | Hir.EVector _ -> true
  | Hir.Index (a, _, _, _)
  | Hir.Field (a, _, _, _, _)
  | Hir.Deref (a, _, _)
  | Hir.Address (a, _, _)
  | Hir.Ptr_add (_, a, _, _, _)
  | Hir.Cast (_, a, _, _) ->
      rooted_in_constant a
  | Hir.Ternary (_, a, b, _, _) -> rooted_in_constant a || rooted_in_constant b
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
  | Ast.Binary ((Ast.Shl | Ast.Shr), _, _, _) -> false
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

let require_place_value c span place =
  match Hir.expr_ty place.expr with
  | Hir.Ptr _ | Hir.ConstPtr _ -> (
      match (place.root, place.path) with
      | Some binding, Some path -> require_place_state binding path c span
      | _ -> Ok ())
  | _ -> Ok ()

let rec check_place (c : context) expr =
  let visible_consts =
    List.filter (fun (name, _, _) -> Option.is_none (lookup_local name c)) c.consts
  in
  let static_index source =
    match
      const_expr ~structs:c.structs ~named_types:c.named_types ~arrays:c.arrays
        visible_consts None ~validate_dead:false source
    with
    | Ok (ty, value) -> Known (ty, value)
    | Error _ -> Dynamic
  in
  match expr with
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b -> Ok { expr = Hir.Local (b, s); root = Some b; path = Some (Exact []) }
      | None -> (
          match lookup n c.arrays with
          | Some (_, (Hir.Array _ as t), _) ->
              Ok { expr = Hir.Const_array (n, t, s); root = None; path = None }
          | Some (_, (Hir.Vec _ as t), values) ->
              Ok { expr = Hir.EVector (values, t, s); root = None; path = None }
          | Some _ -> error s (Printf.sprintf "constant `%s` is not a place" n)
          | None -> (
              match lookup_top_level n c.top_level_bindings with
              | Some { declaration_kind = Top_const; _ } ->
                  error s (Printf.sprintf "constant `%s` is not a place" n)
              | Some { declaration_kind = Top_type; _ } ->
                  error s (Printf.sprintf "type `%s` is not a place" n)
              | Some { declaration_kind = Top_function; _ } ->
                  error s (Printf.sprintf "function `%s` is not a place" n)
              | None -> error s (Printf.sprintf "unknown name `%s`" n))))
  | Ast.Index (a, i, s) -> (
      let* base = check_place c a in
      let* checked_index = check_expr c None i in
      if not (is_int (Hir.expr_ty checked_index)) then
        error s "array index must be an integer"
      else
        match Hir.expr_ty base.expr with
        | Hir.Array (length, e) | Hir.Vec (length, e) -> (
            match static_index i with
            | Known (ty, value)
              when let value = sign_extend_value ty value in
                   value < 0L || value >= Int64.of_int length ->
                error s "array index is out of bounds"
            | Known (ty, value) ->
                let value = sign_extend_value ty value in
                let index = Int64.to_int value in
                let path =
                  match (base.root, base.path) with
                  | Some _, Some (Exact path) -> Some (Exact (path @ [ Element index ]))
                  | Some _, Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                  | _ -> None
                in
                Ok
                  {
                    expr = Hir.Index (base.expr, checked_index, e, s);
                    root = base.root;
                    path;
                  }
            | Dynamic ->
                Ok
                  {
                    expr = Hir.Index (base.expr, checked_index, e, s);
                    root = base.root;
                    path =
                      (match base.path with
                      | Some (Exact path) -> Some (Dynamic_prefix path)
                      | Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                      | None -> None);
                  })
        | Hir.Ptr e | Hir.ConstPtr e ->
            let* () = require_place_value c (Hir.expr_span base.expr) base in
            if match e with Hir.Opaque _ -> true | _ -> false then
              error s "opaque pointers cannot be indexed"
            else if match e with Hir.Void -> true | _ -> false then
              error s "void pointers cannot be indexed"
            else
              Ok
                {
                  expr = Hir.Index (base.expr, checked_index, e, s);
                  root = None;
                  path = None;
                }
        | _ -> error s "cannot index this type")
  | Ast.Field (a, n, s) -> (
      let* base = check_place c a in
      match Hir.expr_ty base.expr with
      | Hir.Struct sn -> (
          match field_info c.structs sn n with
          | Some f ->
              Ok
                {
                  expr = Hir.Field (base.expr, n, f.ty, f.offset, s);
                  root = base.root;
                  path =
                    (match base.path with
                    | Some (Exact path) -> Some (Exact (path @ [ Field n ]))
                    | Some (Dynamic_prefix path) -> Some (Dynamic_prefix path)
                    | None -> None);
                }
          | None -> error s (Printf.sprintf "unknown field `%s`" n))
      | _ -> error s "field access requires a struct")
  | Ast.Deref (e, s) -> (
      let* x = check_expr c None e in
      match Hir.expr_ty x with
      | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
          error s "cannot dereference an opaque pointer"
      | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
          error s "cannot dereference a void pointer"
      | Hir.Ptr t | Hir.ConstPtr t ->
          Ok { expr = Hir.Deref (x, t, s); root = None; path = None }
      | _ -> error s "cannot dereference a non-pointer")
  | e ->
      let* checked = check_expr c None e in
      Ok { expr = checked; root = None; path = None }

and check_expr (c : context) expected = function
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
      if cstr && String.contains v '\000' then
        error s "C string literal cannot contain embedded NUL"
      else
        let value = if cstr then v ^ "\000" else v in
        let* id = intern_string c s value in
        Ok (Hir.EString (id, s))
  | Ast.Ident (n, s) -> (
      match lookup_local n c with
      | Some b ->
          let* () = require_state b [] c s in
          Ok (Hir.Local (b, s))
      | None -> (
          match lookup_top_level n c.top_level_bindings with
          | Some { declaration_kind = Top_type; _ } ->
              error s (Printf.sprintf "`%s` is a type, not a value" n)
          | Some { declaration_kind = Top_function; _ } ->
              error s (Printf.sprintf "`%s` is a function, not a value" n)
          | Some { declaration_kind = Top_const; _ } | None -> (
              match lookup n c.consts with
              | Some (_, t, v) -> Ok (Hir.EInt (v, t, s))
              | None -> (
                  match lookup n c.arrays with
                  | Some (_, (Hir.Array _ as t), _) -> Ok (Hir.Const_array (n, t, s))
                  | Some (_, (Hir.Vec _ as t), values) ->
                      Ok (Hir.EVector (values, t, s))
                  | Some _ -> error s (Printf.sprintf "unknown name `%s`" n)
                  | None -> error s (Printf.sprintf "unknown name `%s`" n)))))
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
          (match (op, expected) with
          | (Ast.Neg | Ast.Bit_not), _ -> expected
          | Ast.Not, Some (Hir.Vec (_, Hir.Bool)) -> expected
          | Ast.Not, _ -> None)
          e
      in
      match op with
      | Ast.Neg | Ast.Bit_not ->
          if not (is_int (Hir.expr_ty te)) then
            error s "integer unary operator requires an integer"
          else Ok (Hir.Unary (op, te, Hir.expr_ty te, s))
      | Ast.Not ->
          let result_ty = Hir.expr_ty te in
          if
            result_ty = Hir.Bool
            || match result_ty with Hir.Vec (_, Hir.Bool) -> true | _ -> false
          then Ok (Hir.Unary (op, te, result_ty, s))
          else error s "logical not requires bool or a bool vector")
  | Ast.Binary (op, l, r, s) ->
      if op = Ast.And || op = Ast.Or then (
        let* a = check_expr c None l in
        let after_left = Sema_flow.snapshot c.flow in
        let dead =
          match (op, a) with
          | Ast.And, Hir.EBool (false, _) | Ast.Or, Hir.EBool (true, _) -> true
          | Ast.And, Hir.EInt (0L, _, _) -> true
          | Ast.Or, Hir.EInt (value, _, _) when value <> 0L -> true
          | _ -> false
        in
        let* b = with_dead_check c dead (fun () -> check_expr c None r) in
        let after_right = Sema_flow.snapshot c.flow in
        Sema_flow.restore c.flow (merge_maps c after_left after_right);
        if Hir.expr_ty a = Hir.Bool && Hir.expr_ty b = Hir.Bool then
          Ok (Hir.Binary (op, a, b, Hir.Bool, s))
        else error s "logical operands must be bool")
      else if op = Ast.Shl || op = Ast.Shr then
        let* a =
          check_expr c
            (match expected with Some (Hir.Int _) -> expected | _ -> None)
            l
        in
        let* b = check_expr c None r in
        let at = Hir.expr_ty a in
        let* () =
          match at with
          | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> Ok ()
          | _ -> error s "shift value must be an integer or integer vector"
        in
        let* () =
          match (at, Hir.expr_ty b) with
          | Hir.Vec (lanes, _), Hir.Vec (count_lanes, Hir.Int _) ->
              if lanes = count_lanes then Ok ()
              else error s "shift count lanes must match the value lanes"
          | Hir.Vec _, Hir.Int _ -> Ok ()
          | Hir.Int _, Hir.Int _ -> Ok ()
          | _, Hir.Vec (_, Hir.Bool) -> error s "shift count must be an integer"
          | (Hir.Int _ | Hir.Vec _), Hir.Vec _ ->
              error s "shift count must be a scalar integer for a scalar value"
          | _ -> error s "shift count must be an integer"
        in
        Ok (Hir.Binary (op, a, b, at, s))
      else
        let* a, b =
          match (unresolved_shape_of l, unresolved_shape_of r) with
          | Some _, None ->
              let* b = check_expr c (operand_type_hint op expected l r) r in
              let* a = check_expr c (Some (Hir.expr_ty b)) l in
              Ok (a, b)
          | _ ->
              let* a = check_expr c (operand_type_hint op expected l r) l in
              let* b = check_expr c (Some (Hir.expr_ty a)) r in
              Ok (a, b)
        in
        let at = Hir.expr_ty a in
        let bt = Hir.expr_ty b in
        let* result_ty =
          binary_result_type ~mismatch:"binary operands must have the same type" s op at
            bt
        in
        if
          (op = Ast.Div || op = Ast.Rem)
          && (not (Sema_flow.checking_dead c.flow))
          && match b with Hir.EInt (v, _, _) -> v = 0L | _ -> false
        then error s "division by zero is not a defined runtime operation"
        else Ok (Hir.Binary (op, a, b, result_ty, s))
  | Ast.Call (fn, args, s) -> check_call c None fn args s
  | Ast.Generic_args (_fn, _, s) ->
      error s "generic specialization is not available in this context"
  | Ast.Cast (k, t, e, s) ->
      let* t = source_ty_in_context c s t in
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
      if cast_legal k from t then Ok (Hir.Cast (k, x, t, s))
      else error s "illegal cast for source and destination widths"
  | Ast.Index (a, i, s) ->
      let* place = check_place c (Ast.Index (a, i, s)) in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      Ok place.expr
  | Ast.Field (a, n, s) ->
      let* place = check_place c (Ast.Field (a, n, s)) in
      let* () =
        match (place.root, place.path) with
        | Some binding, Some path -> require_place_state binding path c s
        | _ -> Ok ()
      in
      Ok place.expr
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
      let* place = check_place c e in
      (match place.root with Some binding -> set_state c binding [] Raw | None -> ());
      match place.expr with
      | Hir.Index (base, _, _, _)
        when match Hir.expr_ty base with Hir.Vec _ -> true | _ -> false ->
          error s "cannot take address of a vector lane"
      | Hir.Local _ | Hir.Deref _ | Hir.Index _ | Hir.Field _ | Hir.Const_array _ ->
          let ty =
            if
              rooted_in_constant place.expr
              || rooted_in_string_literal place.expr
              || rooted_in_readonly_pointer place.expr
            then Hir.ConstPtr (Hir.expr_ty place.expr)
            else Hir.Ptr (Hir.expr_ty place.expr)
          in
          Ok (Hir.Address (place.expr, ty, s))
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
      let* t = source_ty_in_context c s t in
      let* size, _ = layout_diag s c.structs t in
      Ok (Hir.Sizeof (t, size, s))
  | Ast.Alignof (t, s) ->
      let* t = source_ty_in_context c s t in
      let* _, a = layout_diag s c.structs t in
      Ok (Hir.Alignof (t, a, s))
  | Ast.Offsetof (t, n, s) -> (
      let* t = source_ty_in_context c s t in
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
  | Ast.Ternary (q, a, b, s) -> (
      let* tq = check_expr c None q in
      if Hir.expr_ty tq <> Hir.Bool then error s "ternary condition must be bool"
      else
        let before_arms = Sema_flow.snapshot c.flow in
        let condition =
          match tq with
          | Hir.EBool (value, _) -> Some value
          | Hir.EInt (value, _, _) -> Some (value <> 0L)
          | _ -> None
        in
        let* ta =
          with_dead_check c (condition = Some false) (fun () -> check_expr c expected a)
        in
        let after_a = Sema_flow.snapshot c.flow in
        Sema_flow.restore c.flow before_arms;
        let* tb =
          with_dead_check c (condition = Some true) (fun () ->
              check_expr c (Some (Hir.expr_ty ta)) b)
        in
        let after_b = Sema_flow.snapshot c.flow in
        Sema_flow.restore c.flow (merge_maps c after_a after_b);
        let at = Hir.expr_ty ta and bt = Hir.expr_ty tb in
        let result_ty =
          if equal at bt then Some at
          else if compatible at bt then Some bt
          else if compatible bt at then Some at
          else None
        in
        match result_ty with
        | None -> error s "ternary arms have different types"
        | Some _ when at = Hir.Void || bt = Hir.Void ->
            error s "ternary arms cannot have void type"
        | Some ty -> Ok (Hir.Ternary (tq, ta, tb, ty, s)))
  | Ast.Array_lit (_, s) ->
      error s "array literals are only valid in global const declarations"
  | Ast.Struct_lit (source_type, xs, s) -> (
      let* literal_type = source_ty_in_context c s source_type in
      match literal_type with
      | Hir.Struct n -> (
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
                      let* () =
                        ensure_expected (Hir.expr_ty x) f.ty (Ast.expr_span e)
                      in
                      go (x :: acc) ft et
                  | _ -> error s "wrong struct literal arity"
                in
                let* xs = go [] d.fields xs in
                Ok (Hir.Struct_lit (n, xs, literal_type, s)))
      | Hir.Opaque n -> error s (Printf.sprintf "opaque type `%s` is not a struct" n)
      | _ -> error s "struct literal requires a struct type")

and generic_const_argument span = function
  | Ast.Const_arg expression -> Ok expression
  | Ast.Name_arg (name, span) -> Ok (Ast.Ident (name, span))
  | Ast.Type_arg (Ast.Applied_type (name, [ argument ], _)) ->
      let* index = generic_const_argument span argument in
      Ok (Ast.Index (Ast.Ident (name, span), index, span))
  | Ast.Type_arg _ -> error span "expected a const argument"

and check_call c _expected fn args s =
  match fn with
  | Ast.Generic_args (Ast.Ident (name, _), generic_args, application_span) -> (
      match lookup_local name c with
      | Some _ -> error s (Printf.sprintf "`%s` is a value, not a function" name)
      | None -> (
          match List.assoc_opt name c.templates with
          | None ->
              if
                Option.is_some (lookup name c.consts)
                || Option.is_some (lookup name c.arrays)
              then error s (Printf.sprintf "`%s` is a constant, not a function" name)
              else if Option.is_some (List.assoc_opt name c.named_types) then
                error s (Printf.sprintf "`%s` is a type, not a function" name)
              else error s (Printf.sprintf "unknown generic function `%s`" name)
          | Some (Ast.Func { generic_params; _ } as item) ->
              let const_params = const_params generic_params in
              if has_type_params generic_params then
                error s "type-generic call reached ordinary type checking"
              else if List.length generic_args <> List.length const_params then
                error s (Printf.sprintf "wrong number of const arguments to `%s`" name)
              else
                let* cargs = Result_list.map (generic_const_argument s) generic_args in
                let rec eval acc cps actual =
                  match (cps, actual) with
                  | [], [] -> Ok (List.rev acc)
                  | cp :: cs, a :: rest ->
                      let* ct =
                        source_ty_diag c.named_types (Ast.expr_span a) cp.Ast.ty
                      in
                      let* vt, v =
                        const_expr ~structs:c.structs ~named_types:c.named_types
                          ~arrays:c.arrays c.consts (Some ct) a
                      in
                      if equal vt ct then eval ((cp.name, ct, v) :: acc) cs rest
                      else error (Ast.expr_span a) "const argument type mismatch"
                  | _ -> error s "const argument arity mismatch"
                in
                let* values = eval [] const_params cargs in
                let* staged =
                  staged_specialization_identity c.specializations name values
                in
                let* mangled, key =
                  match staged with
                  | None ->
                      let* declaration_id =
                        specialization_declaration_id c.top_level_bindings Top_function
                          s name
                      in
                      Ok
                        ( mangle_specialization name values,
                          function_specialization_key declaration_id values )
                  | Some (origin_id, origin_name, arguments, _, _) ->
                      Ok
                        ( mangle_mixed_specialization origin_name arguments,
                          (Function_specialization, origin_id, arguments) )
                in
                let frame_name, frame_arguments, frame_span =
                  match staged with
                  | None ->
                      ( name,
                        List.map
                          (fun (_, ty, value) -> Diagnostic_const_argument (ty, value))
                          values,
                        application_span )
                  | Some (_, origin_name, _, arguments, span) ->
                      (origin_name, arguments, span)
                in
                let frame =
                  {
                    template_name = frame_name;
                    arguments = frame_arguments;
                    application_span = frame_span;
                  }
                in
                let spec =
                  {
                    key;
                    name = mangled;
                    depth = c.spec_depth;
                    payload =
                      Function_payload
                        { item; substitutions = []; values; staged_args = None };
                    trace = c.spec_trace @ [ frame ];
                    pending_frame = None;
                  }
                in
                let* specialization =
                  Sema_specialization.request c.specializations ~limits:c.limits
                    ~depth:c.spec_depth ~span:s ~description:"const specialization" spec
                in
                let params, ret =
                  match specialization.payload with
                  | Function_payload
                      { item = Ast.Func { params; ret; _ }; substitutions = []; _ } ->
                      (params, ret)
                  | _ -> assert false
                in
                let* ps =
                  Result_list.map
                    (fun (p : Ast.param) ->
                      let* t = source_ty_with_values c.named_types values p.span p.ty in
                      Ok (p.name, t))
                    params
                in
                let* rt = source_ty_with_values c.named_types values s ret in
                let* checked = check_actuals c Reject s ps args in
                Ok (Hir.Call (Hir.User specialization.name, checked, rt, s))
          | Some _ -> error s "const-generic symbol is not a function"))
  | Ast.Ident (name, _) when Names.value_operation name = Some Names.Len ->
      if List.length args <> 1 then error s "builtin `len` expects one argument"
      else
        let argument = List.hd args in
        let* n =
          match argument with
          | Ast.String_lit (cstr, value, _) ->
              if cstr && String.contains value '\000' then
                error s "C string literal cannot contain embedded NUL"
              else Ok (String.length value)
          | _ -> (
              let* value = check_expr c None argument in
              match Hir.expr_ty value with
              | Hir.Array (n, _) -> Ok n
              | _ -> error s "len requires a fixed array or string literal")
        in
        Ok (Hir.EInt (Int64.of_int n, Hir.Int Hir.Usize, s))
  | Ast.Ident (name, _) -> (
      let builtin =
        match Names.value_operation name with
        | Some Names.Rotl -> Some Hir.Rotl
        | Some Names.Rotr -> Some Rotr
        | Some Names.Popcount -> Some Popcount
        | Some Names.Ctz -> Some Ctz
        | Some Names.Clz -> Some Clz
        | Some Names.Len | None -> None
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
        | _ -> (
            if List.length args <> 2 then
              error s (Printf.sprintf "builtin `%s` expects two arguments" name)
            else
              let* a = check_expr c None (List.hd args) in
              let valid_operand =
                is_int (Hir.expr_ty a)
                ||
                match Hir.expr_ty a with
                | Hir.Vec (_, Hir.Int _) -> true
                | _ -> false
              in
              if not valid_operand then
                error s
                  "builtin arguments must be an integer or integer vector and an \
                   integer shift"
              else
                let count = List.hd (List.tl args) in
                match count with
                | Ast.Splat _ -> error s "vector shifts require a scalar integer count"
                | _ -> (
                    let* b2 = check_expr c None count in
                    if is_int (Hir.expr_ty b2) then
                      Ok (Hir.Call (Hir.Builtin b, [ a; b2 ], Hir.expr_ty a, s))
                    else
                      match Hir.expr_ty b2 with
                      | Hir.Vec _ ->
                          error s "vector shifts require a scalar integer count"
                      | _ ->
                          error s
                            "builtin arguments must be an integer or integer vector \
                             and an integer shift"))
      in
      match builtin with
      | Some b
        when Names.reserved_binding_name name
             || Option.is_none (lookup_local name c)
                && Option.is_none (lookup_sig name c) ->
          check_builtin b
      | Some _ | None -> (
          match lookup_local name c with
          | Some _ -> error s (Printf.sprintf "`%s` is a value, not a function" name)
          | None -> (
              match lookup_sig name c with
              | None -> (
                  match List.assoc_opt name c.templates with
                  | Some _ ->
                      error s
                        (Printf.sprintf "generic function `%s` requires arguments" name)
                  | None ->
                      if
                        Option.is_some (lookup name c.consts)
                        || Option.is_some (lookup name c.arrays)
                      then
                        error s
                          (Printf.sprintf "`%s` is a constant, not a function" name)
                      else if Option.is_some (List.assoc_opt name c.named_types) then
                        error s (Printf.sprintf "`%s` is a type, not a function" name)
                      else error s (Printf.sprintf "unknown function `%s`" name))
              | Some sig_ ->
                  if
                    ((not sig_.variadic) && List.length args <> List.length sig_.params)
                    || (sig_.variadic && List.length args < List.length sig_.params)
                  then error s (Printf.sprintf "wrong number of arguments to `%s`" name)
                  else
                    let policy = if sig_.variadic then Promote_variadic else Reject in
                    let* xs = check_actuals c policy s sig_.params args in
                    Ok (Hir.Call (Hir.User name, xs, sig_.ret, s)))))
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
    | (_, expected) :: formal_rest, expression :: actual_rest ->
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
      | Some b -> Ok { target = Hir.ALocal b; root = Some b; path = Some (Exact []) }
      | None -> (
          match lookup_top_level n c.top_level_bindings with
          | Some { declaration_kind = Top_const; _ } ->
              error span (Printf.sprintf "constant `%s` is not assignable" n)
          | Some { declaration_kind = Top_type; _ } ->
              error span (Printf.sprintf "type `%s` is not assignable" n)
          | Some { declaration_kind = Top_function; _ } ->
              error span (Printf.sprintf "function `%s` is not assignable" n)
          | None -> error span (Printf.sprintf "unknown assignment target `%s`" n)))
  | Ast.Target_deref e -> (
      let* x = check_expr c None e in
      if rooted_in_constant x then error (Ast.expr_span e) "cannot modify constant"
      else
        match Hir.expr_ty x with
        | Hir.Ptr (Hir.Opaque _) | Hir.ConstPtr (Hir.Opaque _) ->
            error (Ast.expr_span e) "cannot dereference opaque pointer"
        | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
            error (Ast.expr_span e) "cannot dereference a void pointer"
        | Hir.Ptr _ -> Ok { target = Hir.ADeref x; root = None; path = None }
        | Hir.ConstPtr _ -> error (Ast.expr_span e) "cannot modify read-only pointer"
        | _ -> error (Ast.expr_span e) "deref assignment requires pointer")
  | Ast.Target_index (a, i) -> (
      let* place = check_place c (Ast.Index (a, i, Ast.expr_span a)) in
      let x = place.expr in
      match x with
      | Hir.Index (base, index, _, _) -> (
          if rooted_in_constant x then error (Ast.expr_span a) "cannot modify constant"
          else
            match Hir.expr_ty base with
            | Hir.Ptr Hir.Void | Hir.ConstPtr Hir.Void ->
                error (Ast.expr_span a) "void pointers cannot be indexed"
            | Hir.Array (_, _) | Hir.Vec (_, _) | Hir.Ptr _ ->
                Ok
                  {
                    target = Hir.AIndex (base, index);
                    root = place.root;
                    path = place.path;
                  }
            | Hir.ConstPtr _ ->
                error (Ast.expr_span a) "cannot modify read-only pointer"
            | _ ->
                error (Ast.expr_span a) "index assignment requires aggregate or pointer"
          )
      | _ -> error (Ast.expr_span a) "index assignment requires aggregate or pointer")
  | Ast.Target_field (a, n) -> (
      let* place = check_place c (Ast.Field (a, n, Ast.expr_span a)) in
      let x = place.expr in
      match x with
      | Hir.Field (base, _, _, _, _) -> (
          if rooted_in_constant x then error (Ast.expr_span a) "cannot modify constant"
          else if rooted_in_readonly_pointer x then
            error (Ast.expr_span a) "cannot modify read-only pointer"
          else
            match Hir.expr_ty base with
            | Hir.Struct sn -> (
                match field_info c.structs sn n with
                | Some f ->
                    Ok
                      {
                        target = Hir.AField (base, n, f.offset);
                        root = place.root;
                        path = place.path;
                      }
                | None ->
                    error (Ast.expr_span a) (Printf.sprintf "unknown field `%s`" n))
            | _ -> error (Ast.expr_span a) "field assignment requires struct")
      | _ -> error (Ast.expr_span a) "field assignment requires struct")

let target_ty c = function
  | Hir.ALocal binding -> Some binding.ty
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

let rec stmt_terminates = function
  | Ast.Return _ | Ast.Break _ | Ast.Continue _ -> true
  | Ast.Block (body, _) -> block_terminates body
  | Ast.If (_, then_body, Some else_body, _) ->
      block_terminates then_body && block_terminates else_body
  | Ast.Switch (_, arms, Some default, _) ->
      block_terminates default
      && List.for_all (fun (_, body) -> block_terminates body) arms
  | _ -> false

and block_terminates body =
  match body with
  | [] -> false
  | statement :: rest ->
      if stmt_terminates statement then true else block_terminates rest

let rec check_block (c : context) stmts =
  push c;
  let rec go acc = function
    | [] ->
        let out = List.rev acc in
        let* () = Sema_flow.finish_block_scope c.flow in
        Ok out
    | s :: rest ->
        let before = Sema_flow.snapshot c.flow in
        let* x = check_stmt c s in
        Sema_flow.finish_statement c.flow ~before ~terminates:(stmt_terminates s);
        go (x :: acc) rest
  in
  go [] stmts

and check_stmt (c : context) = function
  | Ast.Let { name; ty; init; raw; span } ->
      let* () = ensure_new_local name c span in
      let* t = source_ty_in_context c span ty in
      let* _ = Sema_limits.validate_object c.limits c.structs span t in
      let* x =
        match init with
        | None -> Ok None
        | Some e ->
            let* v = check_expr c (Some t) e in
            let* () = ensure_expected (Hir.expr_ty v) t (Ast.expr_span e) in
            Ok (Some v)
      in
      let* binding = add_local name t c span in
      if raw then set_state c binding [] Raw;
      if Option.is_some x then mark_init binding c;
      Ok (Hir.Let (binding, x, span))
  | Ast.Assign (t, e, span) ->
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* expected =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid assignment target"
      in
      let* v = check_expr c (Some expected) e in
      let* () = ensure_expected (Hir.expr_ty v) expected span in
      (match (checked_target.root, checked_target.path) with
      | Some binding, Some (Exact path) -> set_state c binding path Full
      | _ -> ());
      Ok (Hir.Assign (target, v, span))
  | Ast.Compound_assign (t, op, e, span) ->
      let* checked_target = check_target c t in
      let target = checked_target.target in
      let* et =
        match target_ty c target with
        | Some t -> Ok t
        | None -> error span "invalid compound assignment target"
      in
      let* () =
        match (checked_target.root, checked_target.path) with
        | Some binding, Some path -> require_place_state binding path c span
        | _ -> Ok ()
      in
      let is_shift = op = Ast.Shl || op = Ast.Shr in
      let* v = check_expr c (if is_shift then None else Some et) e in
      let* () =
        if is_shift then
          let* () =
            match et with
            | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> Ok ()
            | _ -> error span "shift value must be an integer or integer vector"
          in
          match (et, Hir.expr_ty v) with
          | Hir.Vec (lanes, _), Hir.Vec (count_lanes, Hir.Int _) ->
              if lanes = count_lanes then Ok ()
              else error span "shift count lanes must match the value lanes"
          | Hir.Vec _, Hir.Int _ -> Ok ()
          | Hir.Int _, Hir.Int _ -> Ok ()
          | _, Hir.Vec (_, Hir.Bool) -> error span "shift count must be an integer"
          | (Hir.Int _ | Hir.Vec _), Hir.Vec _ ->
              error span "shift count must be a scalar integer for a scalar value"
          | _ -> error span "shift count must be an integer"
        else ensure_expected (Hir.expr_ty v) et span
      in
      if not (is_numeric et) then
        error span "compound assignment requires an integer or vector"
      else (
        (match (checked_target.root, checked_target.path) with
        | Some binding, Some (Exact path) -> set_state c binding path Full
        | _ -> ());
        Ok (Hir.Compound_assign (target, op, v, et, span)))
  | Ast.Return (e, span) ->
      let* () = Sema_flow.validate_return c.flow span in
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
      let* () =
        if Sema_flow.falls_through c.flow then validate_exit_defers c 0 else Ok ()
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
      if Hir.expr_ty tq <> Hir.Bool then error s "if condition must be bool"
      else
        let before = Sema_flow.snapshot c.flow in
        let before_falls = Sema_flow.falls_through c.flow in
        Sema_flow.set_falls_through c.flow before_falls;
        let* ta = check_block c a in
        let ia = Sema_flow.snapshot c.flow in
        let fa = Sema_flow.falls_through c.flow in
        Sema_flow.restore c.flow before;
        Sema_flow.set_falls_through c.flow before_falls;
        let* tb =
          match b with
          | None -> Ok None
          | Some xs ->
              let* x = check_block c xs in
              Ok (Some x)
        in
        let ib = Sema_flow.snapshot c.flow in
        let fb = Sema_flow.falls_through c.flow in
        Sema_flow.restore c.flow
          (match (fa, fb) with
          | true, true -> merge_maps c ia ib
          | true, false -> ia
          | false, true -> ib
          | false, false -> before);
        Sema_flow.set_falls_through c.flow (before_falls && (fa || fb));
        Ok (Hir.If (tq, ta, tb, s))
  | Ast.While (q, b, s) ->
      let* tq = check_expr c None q in
      if Hir.expr_ty tq <> Hir.Bool then error s "while condition must be bool"
      else
        let loop = Sema_flow.begin_loop c.flow in
        let checked = check_block c b in
        Sema_flow.end_loop c.flow;
        let* tb = checked in
        Sema_flow.finish_while c.flow loop ~condition_is_true:(Hir.condition_is_true tq);
        Ok (Hir.While (tq, tb, s))
  | Ast.For (i, q, step, b, s) ->
      push c;
      let checked =
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
              if Hir.expr_ty y = Hir.Bool then Ok (Some y)
              else error (Ast.expr_span x) "for condition must be bool"
        in
        let loop = Sema_flow.begin_loop c.flow in
        let body_result = check_block c b in
        let* tb = body_result in
        Sema_flow.prepare_for_step c.flow loop
          ~body_falls_through:(Hir.block_flow tb).falls_through;
        let* ts =
          match step with
          | None -> Ok None
          | Some x ->
              let* y = check_stmt c x in
              Ok (Some y)
        in
        let unconditional =
          match tq with
          | None -> true
          | Some condition -> Hir.condition_is_true condition
        in
        Sema_flow.finish_for c.flow loop ~unconditional;
        Sema_flow.end_loop c.flow;
        Ok (Hir.For (ti, tq, ts, tb, s))
      in
      pop c;
      checked
  | Ast.Switch (e, arms, d, s) ->
      let* te = check_expr c None e in
      let et = Hir.expr_ty te in
      if not (is_int et || et = Hir.Bool) then
        error s "switch scrutinee must be an integer or bool"
      else
        let before = Sema_flow.snapshot c.flow
        and seen = ref []
        and branch_states = ref [] in
        let before_falls = Sema_flow.falls_through c.flow in
        let branch_falls = ref [] in
        let rec ar acc = function
          | [] -> Ok (List.rev acc)
          | (k, b) :: xs ->
              let* kt, kv =
                const_expr ~structs:c.structs ~named_types:c.named_types
                  ~arrays:c.arrays c.consts (Some et) k
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
                Sema_flow.restore c.flow before;
                Sema_flow.set_falls_through c.flow before_falls;
                let* tb = check_block c b in
                if Sema_flow.falls_through c.flow then
                  branch_states := Sema_flow.snapshot c.flow :: !branch_states;
                branch_falls := Sema_flow.falls_through c.flow :: !branch_falls;
                ar ((tk, tb) :: acc) xs)
        in
        let result = ar [] arms in
        let* ta = result in
        Sema_flow.restore c.flow before;
        Sema_flow.set_falls_through c.flow before_falls;
        let* td =
          match d with
          | None -> Ok None
          | Some x ->
              let* y = check_block c x in
              if Sema_flow.falls_through c.flow then
                branch_states := Sema_flow.snapshot c.flow :: !branch_states;
              branch_falls := Sema_flow.falls_through c.flow :: !branch_falls;
              Ok (Some y)
        in
        (match d with
        | None ->
            branch_states := before :: !branch_states;
            branch_falls := true :: !branch_falls
        | Some _ -> ());
        Sema_flow.restore c.flow
          (match !branch_states with
          | [] -> before
          | first :: rest -> List.fold_left (merge_maps c) first rest);
        Sema_flow.set_falls_through c.flow
          (before_falls && List.exists (fun value -> value) !branch_falls);
        Ok (Hir.Switch (te, ta, td, s))
  | Ast.Break s ->
      let* () = Sema_flow.record_break c.flow s in
      Ok (Hir.Break s)
  | Ast.Continue s ->
      let* () = Sema_flow.record_continue c.flow s in
      Ok (Hir.Continue s)
  | Ast.Defer (xs, s) ->
      let* capture = Sema_flow.begin_defer c.flow s in
      let checked = check_block c xs in
      let* body =
        Sema_flow.finish_defer c.flow capture checked ~falls_through:(fun body ->
            (Hir.block_flow body).falls_through)
      in
      Ok (Hir.Defer (body, s))

let check ?(limits = Limits.default) program =
  let* () =
    if limits.Limits.max_type_nodes >= 0 then Ok ()
    else
      error Span.synthetic
        (Printf.sprintf "budget max_type_nodes must not be negative (profile %s)"
           (Limits.budget_profile_name limits))
  in
  let type_node_account = create_type_node_account limits in
  let specializations = Sema_specialization.create () in
  let declaration = function
    | Ast.Opaque { name; span } | Ast.Struct { name; span; _ } ->
        Some (name, Top_type, span)
    | Ast.Const { name; span; _ } -> Some (name, Top_const, span)
    | Ast.Func { name; span; _ } -> Some (name, Top_function, span)
  in
  let rec validate_declarations next_id seen bindings = function
    | [] -> Ok (List.rev bindings)
    | item :: rest -> (
        match declaration item with
        | None -> validate_declarations next_id seen bindings rest
        | Some (name, kind, span) -> (
            let* () = validate_binding_name span name in
            match String_map.find_opt name seen with
            | None ->
                let binding =
                  {
                    declaration_id = next_id;
                    declaration_name = name;
                    declaration_kind = kind;
                  }
                in
                validate_declarations (next_id + 1)
                  (String_map.add name kind seen)
                  (binding :: bindings) rest
            | Some previous when previous = kind ->
                let label =
                  match kind with
                  | Top_type -> "type"
                  | Top_const -> "const"
                  | Top_function -> "function"
                in
                error span (Printf.sprintf "duplicate %s `%s`" label name)
            | Some _ -> error span (Printf.sprintf "duplicate declaration `%s`" name)))
  in
  let* top_level_bindings =
    validate_declarations 0 String_map.empty [] program.Ast.items
  in
  let* () =
    Sema_invariants.check_declarations
      (List.map
         (fun binding -> (binding.declaration_id, binding.declaration_name))
         top_level_bindings)
    |> phase_invariant
  in
  let rec collect_named_types seen acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Opaque { name; span } :: rest ->
        if String_set.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          collect_named_types (String_set.add name seen) ((name, Opaque_name) :: acc)
            rest
    | Ast.Struct { name; generic_params; span; _ } :: rest ->
        if String_set.mem name seen then
          error span (Printf.sprintf "duplicate type `%s`" name)
        else
          let kind = if generic_params = [] then Struct_name else Generic_struct_name in
          collect_named_types (String_set.add name seen) ((name, kind) :: acc) rest
    | _ :: rest -> collect_named_types seen acc rest
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let* () =
    Result_list.iter
      (function
        | Ast.Struct { generic_params; _ } ->
            validate_generic_params named_types generic_params
        | Ast.Func { generic_params; params; _ } ->
            let* () = validate_generic_params named_types generic_params in
            validate_function_params generic_params params
        | _ -> Ok ())
      program.Ast.items
  in
  let base_structs_src =
    List.filter_map
      (function
        | Ast.Struct { name; generic_params = []; fields; align; _ } -> (
            let fields =
              Result_list.map
                (fun (field : Ast.field) ->
                  let* ty = source_ty named_types field.ty in
                  Ok (field.name, ty))
                fields
            in
            match fields with
            | Ok fields -> Some (name, fields, align)
            | Error _ -> None)
        | _ -> None)
      program.Ast.items
  in
  let base_struct_cache = Hir.struct_layout_cache base_structs_src in
  let base_structs =
    List.filter_map
      (fun (name, _, _) ->
        match Hir.compute_struct_cached base_struct_cache name with
        | Ok definition -> Some definition
        | Error _ -> None)
      base_structs_src
  in
  let* early_consts =
    Sema_constants.resolve_scalar_declarations ~structs:base_structs ~named_types
      ~resolve_type:(fun span ty ->
        source_ty named_types ty
        |> Result.map_error (fun message -> [ Diag.error span message ]))
      ~strict:false program.Ast.items
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr ~check_stmt ~check_target
      ~target_ty ~generic_const_argument
      ~eval_context:(base_structs, named_types, early_consts, [])
      ~top_level_bindings ~limits ~type_node_account specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let rec collect_structs named_types acc = function
    | [] -> Ok (List.rev acc)
    | Ast.Struct { generic_params = _ :: _; align; span; _ } :: rest ->
        let* () = Sema_limits.validate_struct_alignment limits span align in
        collect_structs named_types acc rest
    | Ast.Struct { name; fields; align; span; _ } :: rest ->
        let result =
          let* () = Sema_limits.validate_struct_alignment limits span align in
          let rec collect_fields seen out = function
            | [] -> Ok (List.rev out)
            | (f : Ast.field) :: fields ->
                if String_set.mem f.name seen then
                  error f.span (Printf.sprintf "duplicate field `%s`" f.name)
                else
                  let* ty =
                    source_ty named_types f.ty
                    |> Result.map_error (fun message -> [ Diag.error f.span message ])
                  in
                  collect_fields (String_set.add f.name seen) ((f.name, ty) :: out)
                    fields
          in
          collect_fields String_set.empty [] fields
        in
        let* fields =
          result
          |> trace_result specializations
               (specialization_trace specializations Struct_specialization name)
        in
        collect_structs named_types ((name, fields, align) :: acc) rest
    | _ :: rest -> collect_structs named_types acc rest
  in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let build structs_src =
    let cache = Hir.struct_layout_cache structs_src in
    let rec go acc = function
      | [] -> Ok (List.rev acc)
      | (name, _, _) :: xs ->
          let* s =
            Hir.compute_struct_cached cache name
            |> Result.map_error (fun m -> [ Diag.error Span.synthetic m ])
            |> trace_result specializations
                 (specialization_trace specializations Struct_specialization name)
          in
          go (s :: acc) xs
    in
    go [] structs_src
  in
  let* structs = build structs_src in
  let source_obj span t =
    let* t =
      source_ty named_types t |> Result.map_error (fun m -> [ Diag.error span m ])
    in
    Sema_limits.validate_object limits structs span t
  in
  let map_params convert params =
    Result_list.map
      (fun (param : Ast.param) ->
        let* ty = convert param in
        Ok (param.name, ty))
      params
  in
  let* scalar_consts =
    Sema_constants.resolve_scalar_declarations ~structs ~named_types
      ~resolve_type:source_obj ~strict:true program.Ast.items
  in
  let consts = ref (List.rev scalar_consts) and arrays = ref [] in
  let consts_names =
    ref (String_set.of_list (List.map (fun (n, _, _) -> n) scalar_consts))
  and arrays_names = ref String_set.empty in
  let eval_const_item = function
    | Ast.Const { name; ty; value; span } -> (
        if String_set.mem name !consts_names then Ok ()
        else if String_set.mem name !arrays_names then
          error span (Printf.sprintf "duplicate const `%s`" name)
        else
          let* t = source_obj span ty in
          match (t, value) with
          | Hir.Array (n, elem), Ast.Array_lit (xs, _) ->
              if List.length xs <> n then error span "const array length mismatch"
              else
                let rec values acc = function
                  | [] -> Ok (List.rev acc)
                  | x :: rest ->
                      let* vt, v =
                        const_expr ~structs ~named_types ~arrays:!arrays !consts
                          (Some elem) x
                      in
                      if equal vt elem then values (v :: acc) rest
                      else error (Ast.expr_span x) "const array element type mismatch"
                in
                let* vs = values [] xs in
                arrays := (name, t, vs) :: !arrays;
                arrays_names := String_set.add name !arrays_names;
                Ok ()
          | Hir.Array _, _ -> error span "const array needs a brace-list initializer"
          | (Hir.Vec _ as vector_ty), _ ->
              let* actual_ty, values =
                vector_const_expr ~structs ~named_types ~arrays:!arrays !consts
                  (Some vector_ty) value
              in
              if equal actual_ty vector_ty then (
                arrays := (name, vector_ty, values) :: !arrays;
                arrays_names := String_set.add name !arrays_names;
                Ok ())
              else error span "constant initializer type mismatch"
          | _, Ast.Array_lit _ -> error span "brace-list requires an array type"
          | _, _ ->
              let* vt, v =
                const_expr ~structs ~named_types ~arrays:!arrays !consts (Some t) value
              in
              if equal vt t then (
                consts := (name, t, v) :: !consts;
                consts_names := String_set.add name !consts_names;
                Ok ())
              else error span "constant initializer type mismatch")
    | _ -> Ok ()
  in
  let* () = Result_list.iter eval_const_item program.items in
  let consts_ordered = List.rev !consts and arrays_ordered = List.rev !arrays in
  let* () =
    Sema_invariants.check_const_environment
      ~declared:
        (List.filter_map
           (function Ast.Const { name; _ } -> Some name | _ -> None)
           program.Ast.items)
      ~early:(List.map (fun (name, _, _) -> name) scalar_consts)
      ~consts:(List.map (fun (name, _, _) -> name) consts_ordered)
      ~arrays:(List.map (fun (name, _, _) -> name) arrays_ordered)
    |> phase_invariant
  in
  let* program =
    Sema_substitution.monomorphize_types ~check_expr ~check_stmt ~check_target
      ~target_ty ~generic_const_argument
      ~eval_context:(structs, named_types, consts_ordered, arrays_ordered)
      ~eager_functions:true ~top_level_bindings ~limits ~type_node_account
      specializations program
  in
  let* named_types = collect_named_types String_set.empty [] program.Ast.items in
  let* structs_src = collect_structs named_types [] program.Ast.items in
  let* structs = build structs_src in
  let validate_object span t = Sema_limits.validate_object limits structs span t in
  let source_obj span t =
    let* t = source_ty_diag named_types span t in
    validate_object span t
  in
  let source_return span t =
    let* t = source_ty_diag named_types span t in
    if t = Hir.Void then Ok t else validate_object span t
  in
  let source_params =
    map_params (fun (param : Ast.param) -> source_obj param.span param.ty)
  in
  let sigs = ref [] and declared_functions = ref String_set.empty in
  let* () =
    List.fold_left
      (fun r item ->
        let* () = r in
        match item with
        | Ast.Func { name; params; ret; variadic; linkage; span; generic_params; _ } ->
            let* () = validate_binding_name span name in
            if String_set.mem name !declared_functions then
              error span (Printf.sprintf "duplicate function `%s`" name)
            else if String_set.mem name !arrays_names then
              error span (Printf.sprintf "duplicate declaration `%s`" name)
            else if name = "main" && generic_params <> [] then
              error span "entry point `main` cannot have generic parameters"
            else
              let () = declared_functions := String_set.add name !declared_functions in
              let* () = validate_function_params generic_params params in
              let* () =
                if linkage = Ast.External_c && generic_params <> [] then
                  error span
                    (if has_type_params generic_params then
                       "extern \"C\" functions cannot have type parameters"
                     else "extern \"C\" functions cannot have const parameters")
                else Ok ()
              in
              let* () = validate_generic_params named_types generic_params in
              if variadic && linkage <> Ast.External_c then
                error span "variadic functions require extern \"C\""
              else if generic_params <> [] then Ok ()
              else
                let* ps =
                  map_params
                    (fun (param : Ast.param) -> source_obj param.span param.ty)
                    params
                in
                let* rt = source_return span ret in
                let* () =
                  if linkage = Ast.External_c then
                    validate_extern_c_signature span params ps rt
                  else Ok ()
                in
                sigs := (name, { params = ps; ret = rt; variadic }) :: !sigs;
                Ok ()
        | _ -> Ok ())
      (Ok ()) program.items
  in
  let sigs_ordered = List.rev !sigs in
  let templates =
    List.filter_map
      (fun item ->
        match item with
        | Ast.Func { name; generic_params = _ :: _; _ } -> Some (name, item)
        | _ -> None)
      program.items
  in
  let program_strings = create_string_pool limits in
  let funcs = ref [] in
  let hir_linkage = function
    | Ast.External_c -> Hir.External_c
    | Ast.Internal -> Hir.Internal
  in
  let make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty =
    {
      structs;
      named_types;
      consts =
        (if extra_consts = [] then consts_ordered else extra_consts @ consts_ordered);
      arrays = arrays_ordered;
      signatures = sigs_ordered;
      templates;
      top_level_bindings;
      specializations;
      spec_depth;
      spec_trace;
      flow = Sema_flow.create ~initial_scope:false structs;
      string_pool = program_strings;
      ret_ty;
      limits;
    }
  in
  let hir_params params =
    List.mapi (fun id (name, ty) -> ({ Hir.name; ty; id } : Hir.local)) params
  in
  let check_function_body ~name ~diagnostic_name ~description ~span ~params ~ret ~stmts
      ~linkage ~variadic ~extra_consts ~spec_depth ~spec_trace ~require_return =
    let context = make_context ~extra_consts ~spec_depth ~spec_trace ~ret_ty:ret in
    let* params =
      Result_list.map
        (fun (param_name, param_ty) ->
          let* binding = add_local param_name param_ty context span in
          mark_init binding context;
          Ok binding)
        params
    in
    let* body = check_block context stmts in
    let* () =
      if require_return && ret <> Hir.Void && (Hir.block_flow body).falls_through then
        error span
          (description ^ " `" ^ diagnostic_name
         ^ "` may reach the end without returning")
      else Ok ()
    in
    Ok
      ({ Hir.name; params; ret; body = Hir.Statements body; linkage; variadic }
        : Hir.func)
  in
  let add_func func = funcs := func :: !funcs in
  let check_func = function
    | Ast.Func { name; params; ret; body; linkage; variadic; generic_params = []; span }
      ->
        let trace = specialization_trace specializations Function_specialization name in
        let diagnostic_name =
          specialization_source_name specializations Function_specialization name
        in
        let spec_depth =
          match
            Sema_specialization.find_by_name specializations Function_specialization
              name
          with
          | Some specialization -> specialization.depth + 1
          | None -> 0
        in
        let description = if trace = [] then "function" else "specialized function" in
        let result =
          let* ret = source_return span ret in
          let* params = source_params params in
          let linkage = hir_linkage linkage in
          match body with
          | Ast.Declaration ->
              Ok
                ({
                   Hir.name;
                   params = hir_params params;
                   ret;
                   body = Hir.Declaration;
                   linkage;
                   variadic;
                 }
                  : Hir.func)
          | Ast.Asm raw ->
              Ok
                ({
                   Hir.name;
                   params = hir_params params;
                   ret;
                   body = Hir.Asm raw;
                   linkage;
                   variadic;
                 }
                  : Hir.func)
          | Ast.Statements stmts ->
              check_function_body ~name ~diagnostic_name ~description ~span ~params ~ret
                ~stmts ~linkage ~variadic ~extra_consts:[] ~spec_depth ~spec_trace:trace
                ~require_return:true
        in
        let* func = result |> trace_result specializations trace in
        add_func func;
        Ok ()
    | _ -> Ok ()
  in
  let* () = Result_list.iter check_func program.items in
  let rec materialize () =
    match Sema_specialization.take_pending specializations with
    | None -> Ok ()
    | Some sp -> (
        match sp.payload with
        | Function_payload
            {
              item =
                Ast.Func
                  {
                    params;
                    ret;
                    body = Ast.Statements stmts;
                    linkage;
                    variadic;
                    span;
                    _;
                  };
              substitutions = [];
              values;
              staged_args = _;
            } ->
            let result =
              let* ret = source_ty_with_values named_types values span ret in
              let* ret = if ret = Hir.Void then Ok ret else validate_object span ret in
              let* params =
                Result_list.map
                  (fun (parameter : Ast.param) ->
                    let* ty =
                      source_ty_with_values named_types values parameter.span
                        parameter.ty
                    in
                    let* ty = validate_object parameter.span ty in
                    Ok (parameter.name, ty))
                  params
              in
              check_function_body ~name:sp.name
                ~diagnostic_name:
                  (specialization_source_name specializations Function_specialization
                     sp.name)
                ~description:"specialized function" ~span ~params ~ret ~stmts
                ~linkage:(hir_linkage linkage) ~variadic ~extra_consts:values
                ~spec_depth:(sp.depth + 1) ~spec_trace:sp.trace ~require_return:true
            in
            let* func = result |> trace_result specializations sp.trace in
            add_func func;
            materialize ()
        | Function_payload _ -> materialize ()
        | Struct_payload _ ->
            error Span.synthetic
              "internal error: struct specialization was queued too late")
  in
  let* () = materialize () in
  let* () =
    Sema_invariants.check_materialization
      ~pending:(Sema_specialization.pending_count specializations)
      ~functions:(List.map (fun (func : Hir.func) -> func.name) (List.rev !funcs))
    |> phase_invariant
  in
  let hconsts =
    List.map
      (fun (n, t, v) -> ({ Hir.name = n; ty = t; bits = v } : Hir.const_def))
      consts_ordered
  in
  let harrays =
    List.filter_map
      (fun (n, t, vs) ->
        match t with
        | Hir.Array _ -> Some ({ Hir.name = n; ty = t; elems = vs } : Hir.const_arr_def)
        | _ -> None)
      arrays_ordered
  in
  Ok
    ({
       Hir.structs;
       consts = hconsts;
       const_arrays = harrays;
       funcs = List.rev !funcs;
       strings = List.rev program_strings.reversed;
     }
      : Hir.program)
