#include <stdalign.h>
#include <stdint.h>

typedef long double StorageLongDouble;
typedef int IncompleteArray[];
typedef int ImportedRow[3];
typedef ImportedRow *ImportedRowPointer;
typedef typeof(unsigned long) imported_word;
typedef int imported_stdcall_data;

struct MemberAlignedRecord {
  alignas(16) uint64_t value;
};

struct ImportedFloatRecord {
  uint32_t before;
  StorageLongDouble values[2];
  uint32_t after;
};

extern IncompleteArray imported_incomplete;
extern int (*imported_callbacks[2])(int);
extern ImportedRowPointer imported_row_pointer;
extern imported_word imported_word_value;
extern struct ImportedFloatRecord imported_float_record;
extern imported_stdcall_data imported_read_stdcall_data(void);
extern float *imported_float_pointer(float *value);
extern int imported_callback_value(int (*callback)(int), int value);
extern int imported_row_value(ImportedRowPointer pointer);
extern int imported_incomplete_sum(void);
