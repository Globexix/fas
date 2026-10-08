#include <stdint.h>
#include <stddef.h>

size_t array_slot_count(void);
size_t generic_argument_count(void);
size_t generic_struct_count(void);
size_t large_literal_size(void);
size_t shifted_literal_size(void);
size_t if_expression_size(void);

int main(void) {
  if (array_slot_count() != 10) return 1;
  if (generic_argument_count() != 10) return 2;
  if (generic_struct_count() != 10) return 3;
  if (large_literal_size() != UINT64_C(3000000000)) return 4;
  if (shifted_literal_size() != UINT64_C(8589934592)) return 5;
  if (if_expression_size() != 7) return 6;
  return 0;
}
