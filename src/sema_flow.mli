type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Raw | Partial of (selector * init_state) list
type place_path = Exact of selector list | Dynamic_prefix of selector list
type snapshot
type loop
type defer_capture
type t

val create : initial_scope:bool -> Hir.struct_def list -> t
val lookup_local : string -> t -> binding option
val ensure_new_local : string -> t -> Span.t -> (unit, Diag.t list) result
val add_local : string -> Hir.ty -> t -> Span.t -> (binding, Diag.t list) result
val push : t -> unit
val pop : t -> unit
val set_state : t -> binding -> selector list -> init_state -> unit

val require_state :
  binding -> selector list -> t -> Span.t -> (unit, Diag.t list) result

val require_place_state :
  binding -> place_path -> t -> Span.t -> (unit, Diag.t list) result

val validate_exit_defers : t -> int -> (unit, Diag.t list) result
val mark_init : binding -> t -> unit
val with_dead_check : t -> bool -> (unit -> 'a) -> 'a
val checking_dead : t -> bool
val snapshot : t -> snapshot
val restore : t -> snapshot -> unit
val merge : t -> snapshot -> snapshot -> snapshot
val falls_through : t -> bool
val set_falls_through : t -> bool -> unit
val finish_block_scope : t -> (unit, Diag.t list) result
val finish_statement : t -> before:snapshot -> terminates:bool -> unit
val validate_return : t -> Span.t -> (unit, Diag.t list) result
val begin_loop : t -> loop
val end_loop : t -> unit
val finish_while : t -> loop -> condition_is_true:bool -> unit
val prepare_for_step : t -> loop -> body_falls_through:bool -> unit
val finish_for : t -> loop -> unconditional:bool -> unit
val record_break : t -> Span.t -> (unit, Diag.t list) result
val record_continue : t -> Span.t -> (unit, Diag.t list) result
val begin_defer : t -> Span.t -> (defer_capture, Diag.t list) result

val finish_defer :
  t ->
  defer_capture ->
  ('a, Diag.t list) result ->
  falls_through:('a -> bool) ->
  ('a, Diag.t list) result
