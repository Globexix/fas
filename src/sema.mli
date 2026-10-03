val check :
  ?limits:Limits.t ->
  ?c_aliases:(string * Ast.ty) list ->
  ?c_unsupported:(string * string) list ->
  ?c_nonnull_parameters:(string * int list) list ->
  ?c_records:(string * string * string option) list ->
  Ast.program ->
  (Hir.program, Diag.t list) result
