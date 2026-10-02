#include "c_import_types.h"

IncompleteArray imported_incomplete = {1, 2, 3};

int main(void) {
  if (imported_word_value != 42) return 1;
  if (imported_read_stdcall_data() != 42) return 2;
  if (imported_callback_value(imported_callbacks[0], 40) != 41) return 3;
  if (imported_callback_value(imported_callbacks[1], 40) != 42) return 4;
  if (imported_float_pointer(0) != 0) return 5;
  if (imported_row_value(imported_row_pointer) != 17) return 6;
  if (imported_incomplete[1] != 2) return 7;
  imported_incomplete[0] = 9;
  if (imported_incomplete_sum() != 14) return 8;
  if (sizeof(struct MemberAlignedRecord) != 16) return 9;
  if (_Alignof(struct MemberAlignedRecord) != 16) return 10;
  struct MemberAlignedRecord aligned = {11};
  if (aligned.value != 11) return 11;
  if (sizeof(struct ImportedFloatRecord) != 64) return 12;
  if (_Alignof(struct ImportedFloatRecord) != 16) return 13;
  if (imported_float_record.before != 13) return 14;
  imported_float_record.after = 42;
  if (imported_float_record.after != 42) return 15;
  return 0;
}
