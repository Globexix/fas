#include "phase27_c_import.h"

Phase27IncompleteArray phase27_incomplete = {1, 2, 3};

int main(void) {
  if (phase27_word_value != 42) return 1;
  if (phase27_read_stdcall_data() != 42) return 2;
  if (phase27_callback_value(phase27_callbacks[0], 40) != 41) return 3;
  if (phase27_callback_value(phase27_callbacks[1], 40) != 42) return 4;
  if (phase27_float_pointer(0) != 0) return 5;
  if (phase27_row_value(phase27_row_pointer) != 17) return 6;
  if (phase27_incomplete[1] != 2) return 7;
  phase27_incomplete[0] = 9;
  if (phase27_incomplete_sum() != 14) return 8;
  if (sizeof(struct Phase27AlignasOnly) != 16) return 9;
  if (_Alignof(struct Phase27AlignasOnly) != 16) return 10;
  struct Phase27AlignasOnly aligned = {11};
  if (aligned.value != 11) return 11;
  if (sizeof(struct Phase27FloatRecord) != 64) return 12;
  if (_Alignof(struct Phase27FloatRecord) != 16) return 13;
  if (phase27_float_record.before != 13) return 14;
  phase27_float_record.after = 42;
  if (phase27_float_record.after != 42) return 15;
  return 0;
}
