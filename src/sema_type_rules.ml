let error span message = Error [ Diag.error span message ]

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
    | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div | Ast.Rem | Ast.Bit_and | Ast.Bit_or
    | Ast.Bit_xor ->
        if Sema_numeric.is_numeric left then Ok left
        else error span "arithmetic requires integer or vector operands"
    | Ast.And | Ast.Or ->
        if Sema_numeric.is_truthy left && Sema_numeric.is_truthy right then Ok Hir.Bool
        else error span "logical operands must be scalar"

let variadic_promote expression =
  match Hir.expr_ty expression with
  | Hir.Bool | Hir.Int (Hir.U8 | Hir.U16) ->
      Hir.Cast (Ast.Zext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | Hir.Int (Hir.I8 | Hir.I16) ->
      Hir.Cast (Ast.Sext, expression, Hir.Int Hir.I32, Hir.expr_span expression)
  | _ -> expression
