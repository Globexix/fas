#include <stdint.h>

union FasImportedUnion {
  uint32_t word;
  uint8_t bytes[8];
};

struct FasAnonymousImportedRecord {
  union {
    uint32_t word;
    uint8_t bytes[4];
  };
};

extern void *screens[5];

void fas_union_fill(union FasImportedUnion *value);
void fas_union_report(uint32_t word, uint8_t high);
void fas_union_read_after_write(const union FasImportedUnion *value);
void fas_union_check_table(const union FasImportedUnion *table);
void fas_union_fill_anonymous(struct FasAnonymousImportedRecord *value);
void fas_union_report_anonymous(uint32_t word, uint32_t high);
void fas_union_check_zero_global(void);
void fas_union_check_zero_local(const uint8_t *bytes);
void fas_union_check_zero_union(const union FasImportedUnion *value);
