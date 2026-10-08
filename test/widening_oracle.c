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

void widening_chain_report(uint64_t u8_a, uint64_t u16_b, uint64_t u32_c,
                           uint64_t u16_d, uint64_t reverse_u8_a,
                           uint64_t reverse_u16_b, uint64_t reverse_u32_c,
                           uint64_t reverse_u16_d, int64_t i8_a, int64_t i16_b,
                           int64_t i32_c, int64_t i16_d, int64_t reverse_i8_a,
                           int64_t reverse_i16_b, int64_t reverse_i32_c,
                           int64_t reverse_i16_d) {
  printf("%" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
         " %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64
         " %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64
         " %" PRId64 " %" PRId64 " %" PRId64 " %" PRId64 "\n",
         u8_a, u16_b, u32_c, u16_d, reverse_u8_a, reverse_u16_b,
         reverse_u32_c, reverse_u16_d, i8_a, i16_b, i32_c, i16_d,
         reverse_i8_a, reverse_i16_b, reverse_i32_c, reverse_i16_d);
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
  widening_chain_report(255, 65535, 4294967295, 32768, 255, 65535,
                        4294967295, 32768, -128, -32768, -2147483648, 32767,
                        -128, -32768, -2147483648, 32767);
  return 0;
}
#endif
