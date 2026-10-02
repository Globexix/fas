#include "c_import_types.h"

static int imported_first(int value) { return value + 1; }
static int imported_second(int value) { return value + 2; }
static int imported_row[3] = {17, 23, 31};

int (*imported_callbacks[2])(int) = {imported_first, imported_second};
ImportedRowPointer imported_row_pointer = &imported_row;
imported_word imported_word_value = 42;
struct ImportedFloatRecord imported_float_record = {13, {1.0L, 2.0L}, 7};

imported_stdcall_data imported_read_stdcall_data(void) { return 42; }

float *imported_float_pointer(float *value) { return value; }

int imported_callback_value(int (*callback)(int), int value) {
  return callback(value);
}

int imported_row_value(ImportedRowPointer pointer) { return (*pointer)[0]; }

int imported_incomplete_sum(void) {
  return imported_incomplete[0] + imported_incomplete[1] + imported_incomplete[2];
}
