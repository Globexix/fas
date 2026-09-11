val validate_object :
  Limits.t -> Hir.struct_def list -> Span.t -> Hir.ty -> (Hir.ty, Diag.t list) result

val validate_struct_alignment :
  Limits.t -> Span.t -> int option -> (unit, Diag.t list) result
