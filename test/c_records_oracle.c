#include "c_import/c_records.h"

#include <stddef.h>
#include <stdio.h>
#include <string.h>

FasRecord fas_exported_records[2] = {
  {1, 20, {30, 40}},
  {2, 50, {60, 70}},
};
int32_t fas_completed_values[3] = {4, 5, 6};
int32_t fas_constant_target = 1234;

int main(void) {
  FasAddressEntry table[2] = {
    {"alpha", &fas_constant_target, 77},
    {"omega", &fas_constant_target, 88},
  };
  FasRecord filled_record = {0};
  c_fill_record(&filled_record);
  int32_t filled = filled_record.kind + filled_record.value
      + filled_record.lanes[0] + filled_record.lanes[1];
  FasRecord local = {3, 1001, {4, 5}};
  int32_t local_result = c_check_record(&local);
  int32_t table_result = strcmp(table[0].name, "alpha") == 0
      && strcmp(table[1].name, "omega") == 0
      && table[0].target == table[1].target
      && *table[0].target == 1234 && *table[1].target == 1234
      && table[0].value == 77 && table[1].value == 88 ? 0 : 1;
  int32_t offsets = offsetof(FasRecord, kind) == 0
      && offsetof(FasRecord, value) == 4
      && offsetof(FasRecord, lanes) == 8
      && offsetof(FasAddressEntry, name) == 0
      && offsetof(FasAddressEntry, target) == 8
      && offsetof(FasAddressEntry, value) == 16 ? 0 : 1;
  int32_t exports = offsets;
  if (fas_exported_records[0].kind != 1
      || fas_exported_records[0].value != 20
      || fas_exported_records[0].lanes[0] != 30
      || fas_exported_records[0].lanes[1] != 40
      || fas_exported_records[1].kind != 2
      || fas_exported_records[1].value != 50
      || fas_exported_records[1].lanes[0] != 60
      || fas_exported_records[1].lanes[1] != 70) exports = 1;
  fas_exported_records[1].lanes[1] = 100;
  fas_completed_values[1] = 41;
  int32_t records = fas_exported_records[0].value
      + fas_exported_records[1].lanes[1];
  int32_t completed = fas_completed_values[0] + fas_completed_values[1]
      + fas_completed_values[2];
  if (filled != 1426 || local_result != 0 || table_result != 0
      || exports != 0 || records != 120 || completed != 51) return 1;
  printf("%d %d %d %d %d %d\n", filled, local_result, table_result,
      records, completed, fas_exported_records[1].value);
  return 0;
}
