type named_type_kind =
  | Struct_name
  | Generic_struct_name
  | Opaque_name
  | C_record_name of string * string option
  | Alias_name of Ast.ty
  | Unsupported_name of string * string

type named_types = (string * named_type_kind) list
type const_values = (string * Hir.ty * int64) list

let ( let* ) result continuation =
  match result with Error error -> Error error | Ok value -> continuation value

let error span message = Error [ Diag.error span message ]

let truthiness_help expression ty =
  match (expression, ty) with
  | Ast.Ident (name, _), Hir.Int _ ->
      Some (Printf.sprintf "Fas has no implicit truth values; write `%s != 0`" name)
  | Ast.Ident (name, _), Hir.Addr ->
      Some (Printf.sprintf "Fas has no implicit truth values; write `%s != null`" name)
  | (Ast.Call _ | Ast.Binary _), Hir.Int _ ->
      Some
        (Printf.sprintf "Fas has no implicit truth values; write `%s != 0`"
           (Ast.expr_name expression))
  | _ -> None

let c_pointer_selection_help value type_hint expected =
  let type_name =
    match (type_hint, expected) with
    | Some name, _ when List.mem name Names.scalar_type_names && name <> "void" ->
        Some name
    | None, Some ((Hir.Bool | Hir.Int _) as ty) -> Some (Hir.ty_name ty)
    | _ -> None
  in
  match (value, type_name) with
  | Ast.Ident (name, _), Some type_name ->
      Some (Printf.sprintf "write `%s[%s]`" name type_name)
  | _ -> None

let c_pointer_selection_diagnostic expected = function
  | Ast.C_dereference (Ast.Ident (name, _), Some marker, span)
    when String.starts_with ~prefix:"__fas_c_pointer_cast__:" marker ->
      let cast_type =
        String.sub marker
          (String.length "__fas_c_pointer_cast__:")
          (String.length marker - String.length "__fas_c_pointer_cast__:")
      in
      let help =
        if expected = Some Hir.Addr then Some (Printf.sprintf "write `%s`" name)
        else if List.mem cast_type Names.scalar_type_names then
          Some (Printf.sprintf "write `%s[%s]`" name cast_type)
        else None
      in
      Diag.error ?help span
        (Printf.sprintf "C pointer cast `(%s*)` is not Fas syntax; `addr` is untyped"
           cast_type)
  | Ast.C_dereference (value, type_hint, span) ->
      Diag.error
        ?help:(c_pointer_selection_help value type_hint expected)
        span "Fas has no unary `*`; read through an `addr` with `p[T]`"
  | Ast.C_dot_star (value, span) ->
      Diag.error
        ?help:(c_pointer_selection_help value None expected)
        span "Fas has no `.*`; read through an `addr` with `p[T]`"
  | _ -> assert false

let rec expression_start_span = function
  | Ast.Binary (_, left, _, _)
  | Ast.Unary (_, left, _)
  | Ast.C_dereference (left, _, _)
  | Ast.C_dot_star (left, _) ->
      expression_start_span left
  | Ast.Parenthesized (value, _) -> expression_start_span value
  | expression -> Ast.expr_span expression

let condition_error construct expression ty =
  Diag.error
    ?help:(truthiness_help expression ty)
    (expression_start_span expression)
    (Printf.sprintf "condition of `%s` is `%s`, not `bool`" construct (Hir.ty_name ty))

let logical_operand_error ?help operation side expression ty =
  let help = Option.fold ~none:(truthiness_help expression ty) ~some:Option.some help in
  Diag.error ?help (Ast.expr_span expression)
    (Printf.sprintf "%s operand of `%s` is `%s`, not `bool`" side operation
       (Hir.ty_name ty))

let logical_not_error expression ty =
  let help =
    match (expression, ty) with
    | Ast.Ident (name, _), Hir.Int _ -> Some (Printf.sprintf "write `%s == 0`" name)
    | Ast.Ident (name, _), Hir.Addr -> Some (Printf.sprintf "write `%s == null`" name)
    | _ -> None
  in
  Diag.error ?help (Ast.expr_span expression)
    (Printf.sprintf "operator `!` needs `bool`, got `%s`" (Hir.ty_name ty))

let edit_distance left right =
  let left_length = String.length left in
  let right_length = String.length right in
  let previous = Array.init (right_length + 1) Fun.id in
  let current = Array.make (right_length + 1) 0 in
  for i = 1 to left_length do
    current.(0) <- i;
    for j = 1 to right_length do
      let substitution =
        previous.(j - 1) + if left.[i - 1] = right.[j - 1] then 0 else 1
      in
      current.(j) <- min (min (previous.(j) + 1) (current.(j - 1) + 1)) substitution
    done;
    Array.blit current 0 previous 0 (right_length + 1)
  done;
  previous.(right_length)

let primitive_type_names =
  [
    "bool";
    "void";
    "addr";
    "u8";
    "u16";
    "u32";
    "u64";
    "i8";
    "i16";
    "i32";
    "i64";
    "usize";
    "isize";
  ]

let unknown_type_error visible_types span name =
  let candidates =
    List.sort_uniq String.compare (primitive_type_names @ visible_types)
    |> List.filter (fun candidate -> edit_distance name candidate <= 2)
  in
  let help =
    match candidates with
    | [ candidate ] -> Some (Printf.sprintf "did you mean `%s`?" candidate)
    | _ -> None
  in
  Diag.error ?help span (Printf.sprintf "unknown type `%s`" name)

let similar_name_help visible_names name =
  match
    List.filter
      (fun candidate -> edit_distance name candidate <= 2)
      (List.sort_uniq String.compare visible_names)
  with
  | [ candidate ] -> Some (Printf.sprintf "did you mean `%s`?" candidate)
  | _ -> None

let unknown_type_name message =
  let prefix = "unknown type `" in
  if String.starts_with ~prefix message && String.ends_with ~suffix:"`" message then
    Some
      (String.sub message (String.length prefix)
         (String.length message - String.length prefix - 1))
  else None

let raw_access_needs_type_error ?(index = "i") ?(allow_help = true) base span =
  let help =
    match (allow_help, base) with
    | true, Ast.Ident (name, _) ->
        Some
          (Printf.sprintf "write `%s[T, %s]`, e.g. `%s[u8, %s]`" name index name index)
    | _ -> None
  in
  Diag.error ?help span "raw access on `addr` needs an element type"

let src_int = function
  | Ast.U8 -> Hir.U8
  | U16 -> U16
  | U32 -> U32
  | U64 -> U64
  | I8 -> I8
  | I16 -> I16
  | I32 -> I32
  | I64 -> I64
  | Usize -> Usize
  | Isize -> Isize

let vec_cap_error length (element : Hir.ty) =
  if length > 256 then Some "vector lane count exceeds the portable cap of 256"
  else
    let lane_bits =
      match element with
      | Hir.Bool -> Some 1
      | Hir.Int (Hir.U8 | Hir.I8) -> Some 8
      | Hir.Int (Hir.U16 | Hir.I16) -> Some 16
      | Hir.Int (Hir.U32 | Hir.I32) -> Some 32
      | Hir.Int (Hir.U64 | Hir.I64 | Hir.Usize | Hir.Isize) -> Some 64
      | Hir.Addr | Hir.Handle _ -> Some 64
      | _ -> None
    in
    match lane_bits with
    | Some bits when length * bits > 2048 ->
        Some "vector size exceeds the portable cap of 2048 bits"
    | _ -> None

let rec source_ty named_types = function
  | Ast.Bool -> Ok Hir.Bool
  | Ast.Void -> Ok Hir.Void
  | Ast.Int kind -> Ok (Hir.Int (src_int kind))
  | Ast.Addr -> Ok Hir.Addr
  | Ast.Handle ty ->
      Result.map (fun name -> Hir.Handle name) (handle_target named_types ty)
  | Ast.Array (length, ty) ->
      source_aggregate named_types "array"
        (fun n element -> Hir.Array (n, element))
        length.text ty
  | Ast.Vec (length, ty) -> (
      match
        source_aggregate named_types "vector"
          (fun n element -> Hir.Vec (n, element))
          length.text ty
      with
      | Ok (Hir.Vec (n, element)) -> (
          match vec_cap_error n element with
          | Some message -> Error message
          | None -> Ok (Hir.Vec (n, element)))
      | result -> result)
  | Ast.Named_type (name, _) when Names.reserved_float_name name ->
      Error (Names.reserved_float_message name)
  | Ast.Named_type (name, _) -> (
      match List.assoc_opt name named_types with
      | Some Struct_name -> Ok (Hir.Struct name)
      | Some Generic_struct_name ->
          Error (Printf.sprintf "generic struct `%s` requires type arguments" name)
      | Some Opaque_name -> Ok (Hir.Opaque name)
      | Some (C_record_name (record, None)) -> Ok (Hir.Struct record)
      | Some (C_record_name (_, Some reason)) ->
          Error (Printf.sprintf "C declaration `%s` is not supported: %s" name reason)
      | Some (Alias_name ty) -> source_ty named_types ty
      | Some (Unsupported_name (entity, reason)) ->
          Error (Printf.sprintf "C declaration `%s` is not supported: %s" entity reason)
      | None -> Error (Printf.sprintf "unknown type `%s`" name))
  | Ast.Applied_type _ ->
      Error "generic type application reached ordinary type checking"

and handle_target named_types = function
  | Ast.Named_type (name, _) -> (
      match List.assoc_opt name named_types with
      | Some (C_record_name (record, _)) -> Ok record
      | _ -> (
          match source_ty named_types (Ast.Named_type (name, Span.synthetic)) with
          | Ok (Hir.Opaque name) -> Ok name
          | Ok _ -> Error "handle type argument must be an opaque type"
          | Error message -> Error message))
  | ty -> (
      match source_ty named_types ty with
      | Ok (Hir.Opaque name) -> Ok name
      | Ok _ -> Error "handle type argument must be an opaque type"
      | Error message -> Error message)

and source_aggregate named_types kind make raw element =
  match int_of_string_opt raw with
  | Some length when length < 0 ->
      Error (Printf.sprintf "%s length cannot be negative: `%d`" kind length)
  | Some length ->
      let* element = source_ty named_types element in
      Ok (make length element)
  | None when Sema_numeric.integer_exceeds_max_int raw ->
      Error (Printf.sprintf "%s length `%s` is too large" kind raw)
  | None -> Error (Printf.sprintf "%s length must be an integer constant" kind)

let rec named_type_span name = function
  | Ast.Named_type (candidate, span) when candidate = name -> Some span
  | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> named_type_span name ty
  | Ast.Applied_type (_, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty -> named_type_span name ty
          | _ -> None)
        arguments
  | _ -> None

let rec reserved_float_span = function
  | Ast.Named_type (name, span) when Names.reserved_float_name name -> Some (name, span)
  | Ast.Handle ty | Ast.Array (_, ty) | Ast.Vec (_, ty) -> reserved_float_span ty
  | Ast.Applied_type (_, arguments, _) ->
      List.find_map
        (function
          | Ast.Type_arg ty | Ast.Type_or_index ty -> reserved_float_span ty | _ -> None)
        arguments
  | _ -> None

let source_ty_diag named_types span ty =
  source_ty named_types ty
  |> Result.map_error (fun message ->
      match (unknown_type_name message, reserved_float_span ty) with
      | Some name, _ ->
          [
            unknown_type_error (List.map fst named_types)
              (Option.value ~default:span (named_type_span name ty))
              name;
          ]
      | None, Some (name, type_span) ->
          [ Diag.error type_span (Names.reserved_float_message name) ]
      | None, None ->
          let primary =
            if
              String.starts_with ~prefix:"array length" message
              || String.starts_with ~prefix:"vector length" message
            then
              let rec bad_length_span = function
                | Ast.Array (length, element) | Ast.Vec (length, element) -> (
                    match int_of_string_opt length.text with
                    | Some value when value >= 0 -> bad_length_span element
                    | Some _ | None -> Some length.span)
                | Ast.Handle inner -> bad_length_span inner
                | Ast.Applied_type (_, arguments, _) ->
                    List.find_map
                      (function
                        | Ast.Type_arg ty | Ast.Type_or_index ty -> bad_length_span ty
                        | _ -> None)
                      arguments
                | _ -> None
              in
              Option.value ~default:span (bad_length_span ty)
            else span
          in
          [ Diag.error primary message ])

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let resolve_aggregate_length ?(globals = []) ?(kind = "array") values span length =
  match lookup length values with
  | None when List.mem length globals ->
      error span (Printf.sprintf "global `%s` is not a constant" length)
  | None when Sema_numeric.integer_exceeds_max_int length ->
      error span (Printf.sprintf "%s length `%s` is too large" kind length)
  | None -> Ok length
  | Some (_, ty, value) ->
      if Sema_numeric.is_unsigned ty then
        if Int64.unsigned_compare value (Int64.of_int max_int) > 0 then
          error span
            (Printf.sprintf "%s length `%s` is too large" kind
               (Sema_numeric.unsigned_int64_to_string value))
        else Ok (Int64.to_string value)
      else
        let value = Sema_numeric.sign_extend_value ty value in
        if value < 0L then
          error span (Printf.sprintf "%s length cannot be negative: `%Ld`" kind value)
        else if value > Int64.of_int max_int then
          error span (Printf.sprintf "%s length `%Ld` is too large" kind value)
        else Ok (Int64.to_string value)

let rec source_ty_with_values ?(globals = []) named_types values span = function
  | Ast.Array (length_info, ty) -> (
      let* length =
        resolve_aggregate_length ~kind:"array" ~globals values length_info.span
          length_info.text
      in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then
          error length_info.span
            (Printf.sprintf "array length cannot be negative: `%d`" length)
        else Ok (Hir.Array (length, ty))
      with Failure _ ->
        error length_info.span "array length must be an integer constant")
  | Ast.Vec (length_info, ty) -> (
      let* length =
        resolve_aggregate_length ~kind:"vector" ~globals values length_info.span
          length_info.text
      in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then
          error length_info.span
            (Printf.sprintf "vector length cannot be negative: `%d`" length)
        else
          match vec_cap_error length ty with
          | Some message -> error length_info.span message
          | None -> Ok (Hir.Vec (length, ty))
      with Failure _ ->
        error length_info.span "vector length must be an integer constant")
  | ty -> source_ty_diag named_types span ty

let layout_diag span structs ty =
  Hir.layout structs ty |> Result.map_error (fun message -> [ Diag.error span message ])

let field_info structs name field =
  match
    List.find_opt (fun (struct_def : Hir.struct_def) -> struct_def.name = name) structs
  with
  | None -> None
  | Some struct_def ->
      List.find_opt
        (fun (candidate : Hir.field) -> candidate.name = field)
        struct_def.fields

let compatible actual expected = Hir.ty_equal actual expected
let diagnostic_ty_name ty = Hir.ty_name ty

let function_arity_message name expected actual =
  Printf.sprintf "function `%s` expects %d %s, got %d" name expected
    (if expected = 1 then "argument" else "arguments")
    actual

let generic_arity_message kind name expected actual =
  Printf.sprintf "%s `%s` expects %d generic argument%s, got %d" kind name expected
    (if expected = 1 then "" else "s")
    actual

let constant_initializer_type_message actual expected =
  Printf.sprintf "constant initializer has type `%s`, expected `%s`"
    (diagnostic_ty_name actual) (diagnostic_ty_name expected)

let constant_array_element_type_message actual expected =
  Printf.sprintf "constant array element has type `%s`, expected `%s`"
    (diagnostic_ty_name actual) (diagnostic_ty_name expected)

let shift_operator_name = function
  | Ast.Shl -> "<<"
  | Ast.Shr -> ">>"
  | _ -> assert false

let shift_value_error operator span ty =
  Diag.error span
    (Printf.sprintf
       "left operand of `%s` has type `%s`, expected an integer or integer vector"
       (shift_operator_name operator)
       (diagnostic_ty_name ty))

let shift_count_error operator span ty =
  Diag.error span
    (Printf.sprintf "right operand of `%s` has type `%s`, expected an integer"
       (shift_operator_name operator)
       (diagnostic_ty_name ty))

let shift_count_lanes_error operator span count_ty value_ty =
  Diag.error span
    (Printf.sprintf
       "right operand of `%s` has type `%s`, expected `%s` to match the value"
       (shift_operator_name operator)
       (diagnostic_ty_name count_ty) (diagnostic_ty_name value_ty))

let rotate_value_error name span ty =
  Diag.error span
    (Printf.sprintf "`%s` value must be an integer or integer vector, got `%s`" name
       (diagnostic_ty_name ty))

let rotate_count_error name span ty =
  Diag.error span
    (Printf.sprintf "rotate count for `%s` must be a scalar integer, got `%s`" name
       (diagnostic_ty_name ty))

let cast_name = function
  | Ast.Zext -> "zext"
  | Ast.Sext -> "sext"
  | Ast.Trunc -> "trunc"
  | Ast.Bitcast -> "bitcast"

let cast_integer_value = function
  | Hir.Int _ | Hir.Vec (_, Hir.Int _) -> true
  | Hir.Bool | Hir.Addr | Hir.Handle _ | Hir.Array _ | Hir.Struct _ | Hir.Opaque _
  | Hir.Void | Hir.Vec _ ->
      false

let integer_literal_value = function
  | Ast.Int_lit (raw, _) ->
      Option.map
        (fun value -> (false, value))
        (Result.to_option (Sema_numeric.parse_integer raw))
  | Ast.Unary (Ast.Neg, Ast.Int_lit (raw, _), _) ->
      Option.map
        (fun value -> (true, value))
        (Result.to_option (Sema_numeric.parse_integer raw))
  | _ -> None

let integer_literal_vector_argument_error name expression expected =
  match (integer_literal_value expression, expected) with
  | Some (negative, magnitude), Hir.Vec (_, (Hir.Int _ as element))
    when (if negative then Sema_numeric.fits_negative_literal
          else Sema_numeric.fits_literal)
           element magnitude ->
      Some
        (Diag.error
           ~help:(Printf.sprintf "write `splat(%s)`" (Ast.expr_name expression))
           (Ast.expr_span expression)
           (Printf.sprintf "argument 2 of `%s` is an integer literal, expected `%s`"
              name (diagnostic_ty_name expected)))
  | _ -> None

let cast_error ?expression kind source destination span =
  let name = cast_name kind in
  let source_name, destination_name =
    ( (match expression with
      | Some expression when Option.is_some (integer_literal_value expression) ->
          Printf.sprintf "integer literal `%s`" (Ast.expr_name expression)
      | _ -> Printf.sprintf "`%s`" (diagnostic_ty_name source)),
      Printf.sprintf "`%s`" (diagnostic_ty_name destination) )
  in
  let scalar_integer_pair =
    match (source, destination) with Hir.Int _, Hir.Int _ -> true | _ -> false
  in
  let lane_clause =
    if scalar_integer_pair then "" else " with the same vector lane count"
  in
  let reason =
    match (kind, source, destination) with
    | _, Hir.Addr, Hir.Int Hir.Usize -> "address-to-integer conversion uses `addr_bits`"
    | _, Hir.Int Hir.Usize, Hir.Addr ->
        "integer-to-address conversion uses `addr_from_bits`"
    | Ast.Bitcast, _, _ -> (
        match
          ( Sema_numeric.integer_value_bit_width source,
            Sema_numeric.integer_value_bit_width destination )
        with
        | Some source_bits, Some destination_bits when source_bits <> destination_bits
          ->
            Printf.sprintf "the source is %d bits and the destination is %d bits"
              source_bits destination_bits
        | _ ->
            "both types must be bool, integer, or integer-vector types of equal width")
    | (Ast.Zext | Ast.Sext), _, _
      when not (cast_integer_value source && cast_integer_value destination) ->
        "the source and destination must be integer types"
    | (Ast.Zext | Ast.Sext | Ast.Trunc), _, _ when Hir.ty_equal source destination ->
        Printf.sprintf "the value already has type `%s`" (diagnostic_ty_name source)
    | Ast.Zext, Hir.Int Hir.Usize, Hir.Int Hir.U64 ->
        "the source and destination have the same width; use `bitcast`"
    | (Ast.Zext | Ast.Sext), _, _ ->
        "the destination must be a wider integer type" ^ lane_clause
    | Ast.Trunc, _, _
      when not (cast_integer_value source && cast_integer_value destination) ->
        "the source and destination must be integer types"
    | Ast.Trunc, _, _ -> "the destination must be a narrower integer type" ^ lane_clause
  in
  let message =
    Printf.sprintf "illegal `%s` from %s to %s: %s" name source_name destination_name
      reason
  in
  let help =
    match (kind, source, destination, expression) with
    | _, Hir.Addr, Hir.Int Hir.Usize, Some (Ast.Ident (variable, _)) ->
        Some (Printf.sprintf "write `addr_bits(%s)`" variable)
    | _, Hir.Int Hir.Usize, Hir.Addr, Some (Ast.Ident (variable, _)) ->
        Some (Printf.sprintf "write `addr_from_bits(%s)`" variable)
    | Ast.Trunc, Hir.Int _, Hir.Int _, Some (Ast.Ident (variable, _))
      when Option.get (Sema_numeric.integer_value_bit_width destination)
           > Option.get (Sema_numeric.integer_value_bit_width source) ->
        let extension = if Sema_numeric.is_unsigned source then "zext" else "sext" in
        Some
          (Printf.sprintf "write `%s[%s](%s)`" extension
             (diagnostic_ty_name destination)
             variable)
    | (Ast.Zext | Ast.Sext), Hir.Int _, Hir.Int _, Some (Ast.Ident (variable, _))
      when Option.get (Sema_numeric.integer_value_bit_width destination)
           < Option.get (Sema_numeric.integer_value_bit_width source) ->
        Some
          (Printf.sprintf "write `trunc[%s](%s)`"
             (diagnostic_ty_name destination)
             variable)
    | Ast.Zext, Hir.Int Hir.Usize, Hir.Int Hir.U64, Some (Ast.Ident (variable, _)) ->
        Some (Printf.sprintf "write `bitcast[u64](%s)`" variable)
    | _ -> None
  in
  Diag.error ?help span message

let cast_target_error kind target =
  let expected =
    match kind with
    | Ast.Zext | Ast.Sext -> "an integer or integer-vector type"
    | Ast.Trunc -> "an integer or integer-vector type"
    | Ast.Bitcast -> "a bool, integer, or integer-vector type"
  in
  Printf.sprintf "illegal `%s` target `%s`: expected %s" (cast_name kind)
    (diagnostic_ty_name target) expected

let missing_field_message ?record_name field receiver =
  match receiver with
  | Hir.Struct record ->
      Printf.sprintf "record `%s` has no field `%s`"
        (Option.value ~default:record record_name)
        field
  | _ -> Printf.sprintf "no field `%s` on `%s`" field (diagnostic_ty_name receiver)

let record_field_count_message record expected actual =
  let noun = if expected = 1 then "field" else "fields" in
  Printf.sprintf "record `%s` has %d %s, got %d" record expected noun actual

let array_element_count_message expected actual =
  Printf.sprintf "array of %d elements, got %d" expected actual

let aggregate_count_error_span span n xs =
  Option.fold ~none:span ~some:Ast.expr_span (List.nth_opt xs n)

let ensure_expected ?(context = "value") ?expression ?checked_expression actual expected
    span =
  if compatible actual expected then Ok ()
  else if
    match (checked_expression, expected) with
    | Some (Hir.Address (place, _, _)), Hir.Handle target -> (
        match Hir.expr_ty place with
        | Hir.Struct source -> source <> target
        | _ -> false)
    | _ -> false
  then
    let source_record =
      match checked_expression with
      | Some (Hir.Address (place, _, _)) -> diagnostic_ty_name (Hir.expr_ty place)
      | _ -> assert false
    in
    Diag.error span
      (Printf.sprintf "%s is `addr` to `%s`, expected `%s`" context source_record
         (diagnostic_ty_name expected))
    |> fun diagnostic -> Error [ diagnostic ]
  else
    let help =
      match (expression, actual, expected) with
      | Some (Ast.Ident (name, _)), Hir.Int _, Hir.Int _
        when let actual_bits =
               Option.get (Sema_numeric.integer_value_bit_width actual)
             in
             let expected_bits =
               Option.get (Sema_numeric.integer_value_bit_width expected)
             in
             expected_bits > actual_bits
             && Sema_numeric.is_unsigned actual = Sema_numeric.is_unsigned expected ->
          let extension = if Sema_numeric.is_unsigned actual then "zext" else "sext" in
          Some
            (Printf.sprintf "write `%s[%s](%s)`" extension (diagnostic_ty_name expected)
               name)
      | _ -> None
    in
    Diag.error ?help span
      (Printf.sprintf "%s is `%s`, expected `%s`" context (diagnostic_ty_name actual)
         (diagnostic_ty_name expected))
    |> fun diagnostic -> Error [ diagnostic ]

let binary_operator_name = function
  | Ast.Add -> "+"
  | Ast.Sub -> "-"
  | Ast.Mul -> "*"
  | Ast.Div -> "/"
  | Ast.Rem -> "%"
  | Ast.Bit_and -> "&"
  | Ast.Bit_or -> "|"
  | Ast.Bit_xor -> "^"
  | Ast.Eq -> "=="
  | Ast.Ne -> "!="
  | Ast.Lt -> "<"
  | Ast.Le -> "<="
  | Ast.Gt -> ">"
  | Ast.Ge -> ">="
  | Ast.And -> "&&"
  | Ast.Or -> "||"
  | Ast.Shl -> "<<"
  | Ast.Shr -> ">>"

let is_comparison = function
  | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
  | Ast.Bit_xor | Ast.And | Ast.Or | Ast.Shl | Ast.Shr ->
      false

let ordering_direction = function
  | Ast.Lt | Ast.Le -> Some `Ascending
  | Ast.Gt | Ast.Ge -> Some `Descending
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
  | Ast.Bit_xor | Ast.Eq | Ast.Ne | Ast.And | Ast.Or | Ast.Shl | Ast.Shr ->
      None

let binary_widening_target left_expression right_expression left right =
  match (left, right, left_expression, right_expression) with
  | Hir.Int _, Hir.Int _, Some left_expression, Some right_expression
    when Sema_numeric.is_unsigned left = Sema_numeric.is_unsigned right ->
      let left_width = Option.get (Sema_numeric.integer_value_bit_width left) in
      let right_width = Option.get (Sema_numeric.integer_value_bit_width right) in
      if left_width < right_width then Some (left_expression, left, right)
      else if right_width < left_width then Some (right_expression, right, left)
      else None
  | _ -> None

let binary_widening_help = function
  | Some (Ast.Ident (name, _), actual, expected) ->
      let extension = if Sema_numeric.is_unsigned actual then "zext" else "sext" in
      Some
        (Printf.sprintf "write `%s[%s](%s)`" extension (diagnostic_ty_name expected)
           name)
  | _ -> None

let binary_result_type ?left_expression ?right_expression ?result_expected
    ?(comparison_chain_rewrite_valid = true) span operation left right =
  let result_span expression =
    match expression with Some expression -> Ast.expr_span expression | None -> span
  in
  let comparison_chain () =
    match (left_expression, right_expression) with
    | Some (Ast.Binary (inner_operation, first, middle, _)), Some last
      when is_comparison inner_operation && is_comparison operation ->
        let message =
          match (ordering_direction inner_operation, ordering_direction operation) with
          | Some inner_direction, Some outer_direction
            when inner_direction = outer_direction
                 && left = Hir.Bool && Sema_numeric.is_int right
                 && comparison_chain_rewrite_valid ->
              Printf.sprintf "comparisons cannot be chained; write `%s && %s %s %s`"
                (Ast.expr_name (Ast.Binary (inner_operation, first, middle, span)))
                (Ast.expr_name middle)
                (binary_operator_name operation)
                (Ast.expr_name last)
          | _
            when (operation = Ast.Eq || operation = Ast.Ne)
                 && left = Hir.Bool && right = Hir.Bool ->
              "comparisons cannot be chained; add parentheses"
          | _ -> ""
        in
        if message = "" then None
        else Some (Diag.error (result_span right_expression) message)
    | _ -> None
  in
  let parenthesized_comparison =
    match left_expression with
    | Some (Ast.Parenthesized (Ast.Binary (inner, _, _, _), _)) -> is_comparison inner
    | _ -> false
  in
  match comparison_chain () with
  | Some diagnostic -> Error [ diagnostic ]
  | None
    when parenthesized_comparison
         && (match operation with
           | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> true
           | _ -> false)
         && not
              (Sema_numeric.is_int left || left = Hir.Addr
              || match left with Hir.Vec (_, Hir.Int _) -> true | _ -> false) ->
      let error_span = if left = right then result_span left_expression else span in
      error error_span
        (Printf.sprintf "ordered comparison `%s` needs an integer, got `%s`"
           (binary_operator_name operation)
           (diagnostic_ty_name left))
  | None
    when (operation = Ast.Bit_and || operation = Ast.Bit_or || operation = Ast.Bit_xor)
         && (left = Hir.Addr || right = Hir.Addr) ->
      let offending =
        if left = Hir.Addr && right <> Hir.Addr then left_expression
        else right_expression
      in
      error (result_span offending)
        (Printf.sprintf "bitwise `%s` is not defined for `addr`"
           (binary_operator_name operation))
  | None
    when (operation = Ast.Add || operation = Ast.Sub)
         && (left = Hir.Addr || right = Hir.Addr) ->
      error
        (result_span right_expression)
        (Printf.sprintf
           "address arithmetic for `%s` has operands `%s` and `%s`; expected a scalar \
            integer offset"
           (binary_operator_name operation)
           (diagnostic_ty_name left) (diagnostic_ty_name right))
  | None
    when (operation = Ast.Mul || operation = Ast.Div || operation = Ast.Rem)
         && (left = Hir.Addr || right = Hir.Addr) ->
      let offending = if left = Hir.Addr then left_expression else right_expression in
      error (result_span offending)
        (Printf.sprintf "arithmetic `%s` is not defined for `addr`"
           (binary_operator_name operation))
  | None
    when not
           (Hir.ty_equal left right
           || (operation = Ast.Eq || operation = Ast.Ne)
              && (compatible left right || compatible right left)) ->
      let widening =
        binary_widening_target left_expression right_expression left right
      in
      let offending = Option.map (fun (expression, _, _) -> expression) widening in
      let span =
        match offending with
        | Some expression -> result_span (Some expression)
        | None -> result_span right_expression
      in
      let message =
        Printf.sprintf "operands of `%s` have different types: `%s` and `%s`"
          (binary_operator_name operation)
          (diagnostic_ty_name left) (diagnostic_ty_name right)
      in
      let result_type =
        match operation with
        | Ast.Eq | Ast.Ne | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> Some Hir.Bool
        | _ -> Option.map (fun (_, _, ty) -> ty) widening
      in
      let widening_help =
        match (result_expected, result_type) with
        | None, _ -> binary_widening_help widening
        | Some expected, Some actual when Hir.ty_equal expected actual ->
            binary_widening_help widening
        | _ -> None
      in
      Diag.error ?help:widening_help span message |> fun diagnostic ->
      Error [ diagnostic ]
  | None -> (
      match operation with
      | Ast.Eq | Ast.Ne -> (
          match left with
          | Hir.Vec (lanes, (Hir.Bool | Hir.Int _)) -> Ok (Hir.Vec (lanes, Hir.Bool))
          | Hir.Addr | Hir.Handle _ -> Ok Hir.Bool
          | _ when Sema_numeric.is_scalar left -> Ok Hir.Bool
          | _ ->
              error (result_span left_expression)
                (Printf.sprintf "operator `%s` does not accept `%s`"
                   (binary_operator_name operation)
                   (diagnostic_ty_name left)))
      | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
          if Sema_numeric.is_int left || left = Hir.Addr then Ok Hir.Bool
          else
            match left with
            | Hir.Vec (lanes, Hir.Int _) -> Ok (Hir.Vec (lanes, Hir.Bool))
            | _ ->
                error (result_span left_expression)
                  (Printf.sprintf "ordered comparison `%s` needs an integer, got `%s`"
                     (binary_operator_name operation)
                     (diagnostic_ty_name left)))
      | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor -> (
          if Sema_numeric.is_numeric left then Ok left
          else if left = Hir.Bool then Ok Hir.Bool
          else
            match left with
            | Hir.Vec (_, Hir.Bool) -> Ok left
            | _ ->
                error (result_span left_expression)
                  (Printf.sprintf "operator `%s` needs integer operands, got `%s`"
                     (binary_operator_name operation)
                     (diagnostic_ty_name left)))
      | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem ->
          if Sema_numeric.is_numeric left then Ok left
          else
            error (result_span left_expression)
              (Printf.sprintf "operator `%s` needs integer operands, got `%s`"
                 (binary_operator_name operation)
                 (diagnostic_ty_name left))
      | Ast.And | Ast.Or ->
          if left = Hir.Bool && right = Hir.Bool then Ok Hir.Bool
          else
            let operation = if operation = Ast.And then "&&" else "||" in
            let side, expression, ty =
              if left <> Hir.Bool then ("left", left_expression, left)
              else ("right", right_expression, right)
            in
            let diagnostic =
              match expression with
              | Some expression -> logical_operand_error operation side expression ty
              | None ->
                  Diag.error span
                    (Printf.sprintf "%s operand of `%s` is `%s`, not `bool`" side
                       operation (Hir.ty_name ty))
            in
            Error [ diagnostic ]
      | Ast.Shl | Ast.Shr -> Ok left)

let variadic_promote expression =
  match Hir.expr_ty expression with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | _ -> expression
