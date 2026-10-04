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
  | _ -> None

let condition_error construct expression ty =
  Diag.error
    ?help:(truthiness_help expression ty)
    (Ast.expr_span expression)
    (Printf.sprintf "condition of `%s` is `%s`, not `bool`" construct (Hir.ty_name ty))

let logical_operand_error operation side expression ty =
  Diag.error
    ?help:(truthiness_help expression ty)
    (Ast.expr_span expression)
    (Printf.sprintf "%s operand of `%s` is `%s`, not `bool`" side operation
       (Hir.ty_name ty))

let logical_not_error expression ty =
  let help =
    match (expression, ty) with
    | Ast.Ident (name, _), Hir.Int _ -> Some (Printf.sprintf "write `%s == 0`" name)
    | _ -> None
  in
  Diag.error ?help (Ast.expr_span expression)
    (Printf.sprintf "logical not needs `bool`, got `%s`" (Hir.ty_name ty))

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

let unknown_type_name message =
  let prefix = "unknown type `" in
  if String.starts_with ~prefix message && String.ends_with ~suffix:"`" message then
    Some
      (String.sub message (String.length prefix)
         (String.length message - String.length prefix - 1))
  else None

let raw_access_needs_type_error ?(index = "i") base span =
  let help =
    match base with
    | Ast.Ident (name, _) ->
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
      source_aggregate named_types (fun n element -> Hir.Array (n, element)) length ty
  | Ast.Vec (length, ty) -> (
      match
        source_aggregate named_types (fun n element -> Hir.Vec (n, element)) length ty
      with
      | Ok (Hir.Vec (n, element)) -> (
          match vec_cap_error n element with
          | Some message -> Error message
          | None -> Ok (Hir.Vec (n, element)))
      | result -> result)
  | Ast.Named_type name when Names.reserved_float_name name ->
      Error "reserved for v0.5 floating point"
  | Ast.Named_type name -> (
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
  | Ast.Named_type name -> (
      match List.assoc_opt name named_types with
      | Some (C_record_name (record, _)) -> Ok record
      | _ -> (
          match source_ty named_types (Ast.Named_type name) with
          | Ok (Hir.Opaque name) -> Ok name
          | Ok _ -> Error "handle type argument must be an opaque type"
          | Error message -> Error message))
  | ty -> (
      match source_ty named_types ty with
      | Ok (Hir.Opaque name) -> Ok name
      | Ok _ -> Error "handle type argument must be an opaque type"
      | Error message -> Error message)

and source_aggregate named_types make raw element =
  try
    let length = int_of_string raw in
    if length < 0 then Error "negative aggregate length"
    else
      let* element = source_ty named_types element in
      Ok (make length element)
  with Failure _ -> Error "aggregate length is not a machine integer"

let source_ty_diag named_types span ty =
  source_ty named_types ty
  |> Result.map_error (fun message ->
      match unknown_type_name message with
      | Some name -> [ unknown_type_error (List.map fst named_types) span name ]
      | None -> [ Diag.error span message ])

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let resolve_aggregate_length ?(globals = []) values span length =
  match lookup length values with
  | None when List.mem length globals ->
      error span (Printf.sprintf "global `%s` is not a constant" length)
  | None -> Ok length
  | Some (_, ty, value) ->
      if Sema_numeric.is_unsigned ty then
        if Int64.unsigned_compare value (Int64.of_int max_int) > 0 then
          error span "aggregate length is not a machine integer"
        else Ok (Int64.to_string value)
      else
        let value = Sema_numeric.sign_extend_value ty value in
        if value < 0L then error span "negative aggregate length"
        else if value > Int64.of_int max_int then
          error span "aggregate length is not a machine integer"
        else Ok (Int64.to_string value)

let rec source_ty_with_values ?(globals = []) named_types values span = function
  | Ast.Array (length, ty) -> (
      let* length = resolve_aggregate_length ~globals values span length in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then error span "negative aggregate length"
        else Ok (Hir.Array (length, ty))
      with Failure _ -> error span "aggregate length is not a machine integer")
  | Ast.Vec (length, ty) -> (
      let* length = resolve_aggregate_length ~globals values span length in
      let* ty = source_ty_with_values ~globals named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then error span "negative aggregate length"
        else
          match vec_cap_error length ty with
          | Some message -> error span message
          | None -> Ok (Hir.Vec (length, ty))
      with Failure _ -> error span "aggregate length is not a machine integer")
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

let diagnostic_ty_name ty =
  Hir.ty_name ty |> String.split_on_char ',' |> List.map String.trim
  |> String.concat ","

let ensure_expected ?(context = "value") ?expression actual expected span =
  if compatible actual expected then Ok ()
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

let binary_result_type ~mismatch ?left_expression ?right_expression span operation left
    right =
  if
    not
      (Hir.ty_equal left right
      || (operation = Ast.Eq || operation = Ast.Ne)
         && (compatible left right || compatible right left))
  then error span mismatch
  else
    match operation with
    | Ast.Eq | Ast.Ne -> (
        match left with
        | Hir.Vec (lanes, (Hir.Bool | Hir.Int _)) -> Ok (Hir.Vec (lanes, Hir.Bool))
        | Hir.Addr | Hir.Handle _ -> Ok Hir.Bool
        | _ when Sema_numeric.is_scalar left -> Ok Hir.Bool
        | _ -> error span "equality requires scalar or integer/bool-vector operands")
    | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
        if Sema_numeric.is_int left || left = Hir.Addr then Ok Hir.Bool
        else
          match left with
          | Hir.Vec (lanes, Hir.Int _) -> Ok (Hir.Vec (lanes, Hir.Bool))
          | _ -> error span "ordered comparison requires integer operands")
    | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor -> (
        if Sema_numeric.is_numeric left then Ok left
        else if left = Hir.Bool then Ok Hir.Bool
        else
          match left with
          | Hir.Vec (_, Hir.Bool) -> Ok left
          | _ -> error span "arithmetic requires integer or vector operands")
    | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem ->
        if Sema_numeric.is_numeric left then Ok left
        else error span "arithmetic requires integer or vector operands"
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
    | Ast.Shl | Ast.Shr -> Ok left

let variadic_promote expression =
  match Hir.expr_ty expression with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | _ -> expression
