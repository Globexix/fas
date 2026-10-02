#include "phase27_c_import.h"

static int phase27_first(int value) { return value + 1; }
static int phase27_second(int value) { return value + 2; }
static int phase27_row[3] = {17, 23, 31};

int (*phase27_callbacks[2])(int) = {phase27_first, phase27_second};
Phase27RowPointer phase27_row_pointer = &phase27_row;
phase27_word phase27_word_value = 42;
struct Phase27FloatRecord phase27_float_record = {13, {1.0L, 2.0L}, 7};

phase27_stdcall_data phase27_read_stdcall_data(void) { return 42; }

float *phase27_float_pointer(float *value) { return value; }

int phase27_callback_value(int (*callback)(int), int value) {
  return callback(value);
}

int phase27_row_value(Phase27RowPointer pointer) { return (*pointer)[0]; }

int phase27_incomplete_sum(void) {
  return phase27_incomplete[0] + phase27_incomplete[1] + phase27_incomplete[2];
}
