#include <stdint.h>

union FasPhase23Union {
  uint32_t word;
  uint8_t bytes[8];
};

struct FasPhase23Anonymous {
  union {
    uint32_t word;
    uint8_t bytes[4];
  };
};

extern void *fas_zero_screens[5];

void fas_union_fill(union FasPhase23Union *value);
void fas_union_report(uint32_t word, uint8_t high);
void fas_union_read_after_write(const union FasPhase23Union *value);
void fas_union_check_table(const union FasPhase23Union *table);
void fas_union_fill_anonymous(struct FasPhase23Anonymous *value);
void fas_union_report_anonymous(uint32_t word, uint32_t high);
void fas_union_check_zero_global(void);
void fas_union_check_zero_local(const uint8_t *bytes);
void fas_union_check_zero_union(const union FasPhase23Union *value);
