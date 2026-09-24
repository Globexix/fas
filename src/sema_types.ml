type named_type_kind = Struct_name | Generic_struct_name | Opaque_name
type named_types = (string * named_type_kind) list
type const_values = (string * Hir.ty * int64) list

let ( let* ) result continuation =
  match result with Error error -> Error error | Ok value -> continuation value

let error span message = Error [ Diag.error span message ]

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
      | Hir.Ptr _ | Hir.ConstPtr _ -> Some 64
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
  | Ast.Ptr ty ->
      let* ty = source_ty named_types ty in
      Ok (Hir.Ptr ty)
  | Ast.Ptr_const ty ->
      let* ty = source_ty named_types ty in
      Ok (Hir.ConstPtr ty)
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
  | Ast.Named_type name -> (
      match List.assoc_opt name named_types with
      | Some Struct_name -> Ok (Hir.Struct name)
      | Some Generic_struct_name ->
          Error (Printf.sprintf "generic struct `%s` requires type arguments" name)
      | Some Opaque_name -> Ok (Hir.Opaque name)
      | None -> Error (Printf.sprintf "unknown type `%s`" name))
  | Ast.Applied_type _ ->
      Error "generic type application reached ordinary type checking"

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
  |> Result.map_error (fun message -> [ Diag.error span message ])

let lookup name table = List.find_opt (fun (entry, _, _) -> entry = name) table

let resolve_aggregate_length values span length =
  match lookup length values with
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

let rec source_ty_with_values named_types values span = function
  | Ast.Ptr ty ->
      let* ty = source_ty_with_values named_types values span ty in
      Ok (Hir.Ptr ty)
  | Ast.Ptr_const ty ->
      let* ty = source_ty_with_values named_types values span ty in
      Ok (Hir.ConstPtr ty)
  | Ast.Array (length, ty) -> (
      let* length = resolve_aggregate_length values span length in
      let* ty = source_ty_with_values named_types values span ty in
      try
        let length = int_of_string length in
        if length < 0 then error span "negative aggregate length"
        else Ok (Hir.Array (length, ty))
      with Failure _ -> error span "aggregate length is not a machine integer")
  | Ast.Vec (length, ty) -> (
      let* length = resolve_aggregate_length values span length in
      let* ty = source_ty_with_values named_types values span ty in
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

let compatible actual expected =
  Hir.ty_equal actual expected
  ||
  match (actual, expected) with
  | Hir.Ptr actual, Hir.ConstPtr expected -> Hir.ty_equal actual expected
  | _ -> false

let ensure_expected actual expected span =
  if compatible actual expected then Ok ()
  else
    error span
      (Printf.sprintf "type mismatch: expected %s, got %s" (Hir.ty_name expected)
         (Hir.ty_name actual))

let binary_result_type ~mismatch span operation left right =
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
        | _ when Sema_numeric.is_scalar left -> Ok Hir.Bool
        | _ -> error span "equality requires scalar or integer/bool-vector operands")
    | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge -> (
        if Sema_numeric.is_int left then Ok Hir.Bool
        else
          match left with
          | Hir.Vec (lanes, Hir.Int _) -> Ok (Hir.Vec (lanes, Hir.Bool))
          | _ -> error span "ordered comparison requires integer operands")
    | Ast.Bit_and | Ast.Bit_or | Ast.Bit_xor ->
        if Sema_numeric.is_numeric left then Ok left
        else if left = Hir.Bool then Ok Hir.Bool
        else error span "arithmetic requires integer or vector operands"
    | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem ->
        if Sema_numeric.is_numeric left then Ok left
        else error span "arithmetic requires integer or vector operands"
    | Ast.And | Ast.Or ->
        if left = Hir.Bool && right = Hir.Bool then Ok Hir.Bool
        else error span "logical operands must be bool"
    | Ast.Shl | Ast.Shr -> Ok left

let variadic_promote expression =
  match Hir.expr_ty expression with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | _ -> expression
