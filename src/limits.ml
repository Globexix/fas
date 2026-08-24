type t = {
  max_tokens : int;
  max_nesting : int;
  max_asm_bytes : int;
  max_specializations : int;
  max_specialization_depth : int;
  max_aggregate_elements : int;
}

let default =
  {
    max_tokens = 1_000_000;
    max_nesting = 128;
    max_asm_bytes = 4_000_000;
    max_specializations = 10_000;
    max_specialization_depth = 64;
    max_aggregate_elements = 1_000_000;
  }
