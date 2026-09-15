type budget_version = V0_15

type t = {
  max_tokens : int;
  max_nesting : int;
  max_asm_bytes : int;
  max_interned_string_bytes : int;
  max_rendered_ir_bytes : int;
  max_rendered_ast_bytes : int;
  max_specializations : int;
  max_specialization_depth : int;
  max_aggregate_elements : int;
  max_object_alignment : int;
  max_object_size : int;
}

let budget_version_name = function V0_15 -> "0.15"
let default_budget_version = V0_15

let for_budget_version = function
  | V0_15 ->
      {
        max_tokens = 1_000_000;
        max_nesting = 128;
        max_asm_bytes = 4_000_000;
        max_interned_string_bytes = 4_000_000;
        max_rendered_ir_bytes = 4_000_000;
        max_rendered_ast_bytes = 4_000_000;
        max_specializations = 10_000;
        max_specialization_depth = 64;
        max_aggregate_elements = 1_000_000;
        max_object_alignment = 1_048_576;
        max_object_size = 1_073_741_824;
      }

let default = for_budget_version default_budget_version
