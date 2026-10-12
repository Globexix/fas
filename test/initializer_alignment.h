#include <stdint.h>
typedef struct __attribute__((packed)) FasPackedInit {
  char tag;
  int64_t integer;
  double decimal;
  float fraction;
} FasPackedInit;
typedef struct FasOrdinaryInit {
  char tag;
  int64_t integer;
  double decimal;
  float fraction;
} FasOrdinaryInit;
