val compatible : Hir.ty -> Hir.ty -> bool
val ensure_expected : Hir.ty -> Hir.ty -> Span.t -> (unit, Diag.t list) result

val binary_result_type :
  mismatch:string ->
  Span.t ->
  Ast.binop ->
  Hir.ty ->
  Hir.ty ->
  (Hir.ty, Diag.t list) result

val variadic_promote : Hir.expr -> Hir.expr
