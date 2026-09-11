val const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  ?validate_dead:bool ->
  Ast.expr ->
  (Hir.ty * int64, Diag.t list) result

val vector_const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  Ast.expr ->
  (Hir.ty * int64 list, Diag.t list) result
