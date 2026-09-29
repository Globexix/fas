val check :
  ?limits:Limits.t ->
  ?c_aliases:(string * Ast.ty) list ->
  ?c_unsupported:(string * string) list ->
  Ast.program ->
  (Hir.program, Diag.t list) result
