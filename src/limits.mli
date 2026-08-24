type t = {
  max_tokens : int;
  max_nesting : int;
  max_asm_bytes : int;
  max_specializations : int;
  max_specialization_depth : int;
  max_aggregate_elements : int;
}

val default : t
