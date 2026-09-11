type budget_version = V0_15

type t = {
  max_tokens : int;
  max_nesting : int;
  max_asm_bytes : int;
  max_specializations : int;
  max_specialization_depth : int;
  max_aggregate_elements : int;
  max_object_alignment : int;
  max_object_size : int;
}

val budget_version_name : budget_version -> string
val default_budget_version : budget_version
val for_budget_version : budget_version -> t
val default : t
