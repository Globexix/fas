val const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  ?resolve:(check_only:bool -> string -> Span.t -> (Hir.ty * int64, Diag.t list) result) ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  ?validate_dead:bool ->
  Ast.expr ->
  (Hir.ty * int64, Diag.t list) result

val shuffle_indices_in_range : Hir.ty -> int -> int64 list -> bool

val vector_const_expr :
  ?structs:Hir.struct_def list ->
  ?named_types:Sema_types.named_types ->
  ?arrays:(string * Hir.ty * int64 list) list ->
  ?resolve:(check_only:bool -> string -> Span.t -> (Hir.ty * int64, Diag.t list) result) ->
  Sema_types.const_values ->
  Hir.ty option ->
  ?check_only:bool ->
  Ast.expr ->
  (Hir.ty * int64 list, Diag.t list) result

val resolve_scalar_declarations :
  structs:Hir.struct_def list ->
  named_types:Sema_types.named_types ->
  resolve_type:(Span.t -> Ast.ty -> (Hir.ty, Diag.t list) result) ->
  strict:bool ->
  Ast.item list ->
  (Sema_types.const_values, Diag.t list) result
