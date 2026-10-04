type named_type_kind =
  | Struct_name
  | Generic_struct_name
  | Opaque_name
  | C_record_name of string * string option
  | Alias_name of Ast.ty
  | Unsupported_name of string * string

type named_types = (string * named_type_kind) list
type const_values = (string * Hir.ty * int64) list

val source_ty : named_types -> Ast.ty -> (Hir.ty, string) result
val handle_target : named_types -> Ast.ty -> (string, string) result
val source_ty_diag : named_types -> Span.t -> Ast.ty -> (Hir.ty, Diag.t list) result
val vec_cap_error : int -> Hir.ty -> string option
val condition_error : string -> Ast.expr -> Hir.ty -> Diag.t
val logical_operand_error : string -> string -> Ast.expr -> Hir.ty -> Diag.t
val logical_not_error : Ast.expr -> Hir.ty -> Diag.t
val raw_access_needs_type_error : ?index:string -> Ast.expr -> Span.t -> Diag.t
val unknown_type_error : string list -> Span.t -> string -> Diag.t
val unknown_type_name : string -> string option

val resolve_aggregate_length :
  ?globals:string list ->
  const_values ->
  Span.t ->
  string ->
  (string, Diag.t list) result

val source_ty_with_values :
  ?globals:string list ->
  named_types ->
  const_values ->
  Span.t ->
  Ast.ty ->
  (Hir.ty, Diag.t list) result

val layout_diag :
  Span.t -> Hir.struct_def list -> Hir.ty -> (int * int, Diag.t list) result

val field_info : Hir.struct_def list -> string -> string -> Hir.field option
val compatible : Hir.ty -> Hir.ty -> bool
val ensure_expected : Hir.ty -> Hir.ty -> Span.t -> (unit, Diag.t list) result

val binary_result_type :
  mismatch:string ->
  ?left_expression:Ast.expr ->
  ?right_expression:Ast.expr ->
  Span.t ->
  Ast.binop ->
  Hir.ty ->
  Hir.ty ->
  (Hir.ty, Diag.t list) result

val variadic_promote : Hir.expr -> Hir.expr
