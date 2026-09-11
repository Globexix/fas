module State_map : Map.S with type key = int

type binding = Hir.local = { name : string; ty : Hir.ty; id : int }
type selector = Field of string | Element of int
type init_state = Uninit | Full | Raw | Partial of (selector * init_state) list
type place_path = Exact of selector list | Dynamic_prefix of selector list
type deferred_requirement = binding * selector list * Span.t

type checked_defer = {
  requirements : deferred_requirement list;
  effects : (binding * init_state) list;
  falls_through : bool;
}

type loop_init_flow = {
  keep_defer_depth : int;
  mutable break_states : init_state State_map.t list;
  mutable continue_states : init_state State_map.t list;
}

type t = {
  structs : Hir.struct_def list;
  locals : (string, binding) Hashtbl.t list ref;
  mutable initialized : init_state State_map.t;
  mutable next_binding_id : int;
  mutable loop_depth : int;
  mutable loop_init_flows : loop_init_flow list;
  mutable in_defer : bool;
  mutable collecting_defer : deferred_requirement list option;
  mutable defer_scopes : checked_defer list list;
  mutable falls_through : bool;
  mutable checking_dead : bool;
}

val create : initial_scope:bool -> Hir.struct_def list -> t
val lookup_local : string -> t -> binding option
val ensure_new_local : string -> t -> Span.t -> (unit, Diag.t list) result
val add_local : string -> Hir.ty -> t -> Span.t -> (binding, Diag.t list) result
val push : t -> unit
val pop : t -> unit
val state_of : t -> binding -> init_state
val set_state : t -> binding -> selector list -> init_state -> unit

val require_state :
  binding -> selector list -> t -> Span.t -> (unit, Diag.t list) result

val require_place_state :
  binding -> place_path -> t -> Span.t -> (unit, Diag.t list) result

val merge_maps :
  t -> init_state State_map.t -> init_state State_map.t -> init_state State_map.t

val validate_defer_list : t -> checked_defer list -> (bool, Diag.t list) result
val exit_defer_state : t -> int -> (init_state State_map.t option, Diag.t list) result
val validate_exit_defers : t -> int -> (unit, Diag.t list) result

val merge_flow_states :
  t -> init_state State_map.t list -> init_state State_map.t option

val mark_init : binding -> t -> unit
val with_dead_check : t -> bool -> (unit -> 'a) -> 'a
