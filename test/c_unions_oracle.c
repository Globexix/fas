#include <stdio.h>
#include "c_unions.h"

int main(void) {
  union FasPhase23Union value = {0};
  const union FasPhase23Union table[2] = {{0x11223344}, {0x55667788}};
  for (unsigned i = 0; i < 8; ++i) value.bytes[i] = (uint8_t)(i + 1);
  printf("%u %u\n", value.word, (unsigned)value.bytes[7]);
  value.word = 0x76543210;
  printf("%u %u\n", (unsigned)value.bytes[0], (unsigned)value.bytes[3]);
  printf("%u %u %u %u\n", table[0].word, (unsigned)table[0].bytes[4],
         table[1].word, (unsigned)table[1].bytes[4]);
  return 0;
}
