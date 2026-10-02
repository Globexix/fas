#include <stdio.h>
#include "c_unions.h"

void *screens[5] = {0};

int main(void) {
  union FasImportedUnion value = {0};
  const union FasImportedUnion table[2] = {{0x11223344}, {0x55667788}};
  for (unsigned i = 0; i < 8; ++i) value.bytes[i] = (uint8_t)(i + 1);
  printf("%u %u\n", value.word, (unsigned)value.bytes[7]);
  value.word = 0x76543210;
  printf("%u %u\n", (unsigned)value.bytes[0], (unsigned)value.bytes[3]);
  printf("%u %u %u %u\n", table[0].word, (unsigned)table[0].bytes[4],
         table[1].word, (unsigned)table[1].bytes[4]);
  struct FasAnonymousImportedRecord anonymous = {0};
  anonymous.word = 0x01020304u;
  printf("anonymous %u %u\n", anonymous.word,
         (unsigned)anonymous.bytes[3]);
  anonymous.bytes[0] = 170;
  anonymous.bytes[3] = 187;
  printf("anonymous %u %u\n", anonymous.word,
         (unsigned)anonymous.bytes[3]);
  int global_zero = 1;
  const unsigned char *global_bytes = (const unsigned char *)screens;
  for (unsigned i = 0; i < sizeof(screens); ++i)
    if (global_bytes[i] != 0) global_zero = 0;
  printf("global zero %d\n", global_zero);
  uint8_t scratch[16] = {0};
  scratch[3] = 11;
  uint8_t chars[5] = {0};
  int local_zero = 1;
  for (unsigned i = 0; i < sizeof(chars); ++i)
    if (chars[i] != 0) local_zero = 0;
  printf("local zero %d\n", local_zero);
  union FasImportedUnion zero_union = {0};
  int union_zero = 1;
  for (unsigned i = 0; i < sizeof(zero_union.bytes); ++i)
    if (zero_union.bytes[i] != 0) union_zero = 0;
  printf("union zero %d\n", union_zero);
  return 0;
}
