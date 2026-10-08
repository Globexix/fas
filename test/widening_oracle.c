#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>

uint32_t widening_c_take_u32(uint32_t value) {
  return value + 1;
}

void widening_report(uint32_t to_u32, int16_t to_i16, int64_t to_i64,
                     uint32_t c_result, uint64_t position, uint32_t aggregate,
                     uint32_t fas_call, uint32_t case_value, uint32_t branch_sum,
                     uint32_t comparisons, uint32_t wide, uint32_t array_value,
                     uint32_t global_value, uint32_t global_field,
                     uint32_t narrow_product) {
  printf("%" PRIu32 " %" PRId16 " %" PRId64 " %" PRIu32 " %" PRIu64
         " %" PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32
         " %" PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32 " %" PRIu32 "\n",
         to_u32, to_i16, to_i64, c_result, position, aggregate, fas_call,
         case_value, branch_sum, comparisons, wide, array_value, global_value,
         global_field, narrow_product);
}

void widening_branch_report(uint64_t zero_low, uint64_t zero_high, int64_t sign_low,
                            int64_t sign_high) {
  printf("%" PRIu64 " %" PRIu64 " %" PRId64 " %" PRId64 "\n", zero_low,
         zero_high, sign_low, sign_high);
}

#ifdef WIDENING_ORACLE
int main(void) {
  widening_report(255, 255, -1, 256, 255, 765, 255, 7, 510, 1, 255, 255, 255, 255,
                  500000);
  return 0;
}
#endif

#ifdef WIDENING_BRANCH_ORACLE
int main(void) {
  widening_branch_report(0, 65535, -32768, 32767);
  return 0;
}
#endif
