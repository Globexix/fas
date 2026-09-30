#include <stdint.h>

union FasPhase23Union {
  uint32_t word;
  uint8_t bytes[8];
};

void fas_union_fill(union FasPhase23Union *value);
void fas_union_report(uint32_t word, uint8_t high);
void fas_union_read_after_write(const union FasPhase23Union *value);
void fas_union_check_table(const union FasPhase23Union *table);
