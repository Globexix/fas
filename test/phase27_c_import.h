#include <stdalign.h>
#include <stdint.h>

typedef long double Phase27StorageLongDouble;
typedef int Phase27IncompleteArray[];
typedef int Phase27Row[3];
typedef Phase27Row *Phase27RowPointer;
typedef typeof(unsigned long) phase27_word;
typedef int phase27_stdcall_data;

struct Phase27AlignasOnly {
  alignas(16) uint64_t value;
};

struct Phase27FloatRecord {
  uint32_t before;
  Phase27StorageLongDouble values[2];
  uint32_t after;
};

extern Phase27IncompleteArray phase27_incomplete;
extern int (*phase27_callbacks[2])(int);
extern Phase27RowPointer phase27_row_pointer;
extern phase27_word phase27_word_value;
extern struct Phase27FloatRecord phase27_float_record;
extern phase27_stdcall_data phase27_read_stdcall_data(void);
extern float *phase27_float_pointer(float *value);
extern int phase27_callback_value(int (*callback)(int), int value);
extern int phase27_row_value(Phase27RowPointer pointer);
extern int phase27_incomplete_sum(void);
