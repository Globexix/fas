#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void program_value(double x) {
  uint64_t bits;
  memcpy(&bits, &x, sizeof(bits));
  printf("%016lx\n", (unsigned long)bits);
}
void program_count(uint32_t x) { printf("%u\n", x); }
void program_header(int n) { printf("P3\n%d %d\n255\n", n, n); }
void program_pixel(uint32_t x) { printf("%u %u %u\n", x, x, x); }
void programs_run(int kind, int size);
int main(int argc, char **argv) {
  if (argc != 3) return 2;
  programs_run(atoi(argv[1]), atoi(argv[2]));
  return ferror(stdout) ? 1 : 0;
}
