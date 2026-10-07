#include <stdint.h>
#include <stdio.h>

static int32_t fas_oracle_callback(int32_t value) {
  return value + 1;
}

int main(void) {
  printf("%d\n", fas_oracle_callback(41));
  return 0;
}
