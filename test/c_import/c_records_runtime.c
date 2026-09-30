#include "c_records.h"

#include <stddef.h>
#include <string.h>

static int layout_ok(void) {
  return offsetof(FasRecord, kind) == 0
      && offsetof(FasRecord, value) == 4
      && offsetof(FasRecord, lanes) == 8
      && offsetof(FasAddressEntry, name) == 0
      && offsetof(FasAddressEntry, target) == 8
      && offsetof(FasAddressEntry, value) == 16;
}

void c_fill_record(FasRecord *record) {
  record->kind = 90;
  record->value = 1300;
  record->lanes[0] = 17;
  record->lanes[1] = 19;
}

int32_t c_check_record(const FasRecord *record) {
  return record->kind == 3 && record->value == 1001
      && record->lanes[0] == 4 && record->lanes[1] == 5 ? 0 : 1;
}

int32_t c_check_table(const FasAddressEntry *entries) {
  return strcmp(entries[0].name, "alpha") == 0
      && strcmp(entries[1].name, "omega") == 0
      && entries[0].target == &fas_constant_target
      && entries[1].target == &fas_constant_target
      && *entries[0].target == 1234 && *entries[1].target == 1234
      && entries[0].value == 77 && entries[1].value == 88 ? 0 : 1;
}

int32_t c_check_link_table(void) {
  return fas_link_table[0].link == &fas_link_table[1]
      && fas_link_table[0].link->link == &fas_link_table[2]
      && fas_link_table[0].link->link->link == &fas_link_table[0]
      && fas_link_table[0].link->value + fas_link_table[0].link->link->value
          + fas_link_table[0].link->link->link->value == 60 ? 0 : 1;
}

int32_t c_check_and_mutate_exports(void) {
  if (!layout_ok()) return 1;
  if (fas_exported_records[0].kind != 1
      || fas_exported_records[0].value != 20
      || fas_exported_records[0].lanes[0] != 30
      || fas_exported_records[0].lanes[1] != 40
      || fas_exported_records[1].kind != 2
      || fas_exported_records[1].value != 50
      || fas_exported_records[1].lanes[0] != 60
      || fas_exported_records[1].lanes[1] != 70) return 2;
  fas_exported_records[1].lanes[1] = 100;
  fas_completed_values[1] = 41;
  return 0;
}
