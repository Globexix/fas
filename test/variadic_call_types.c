#include <stdarg.h>
#include <stdio.h>

int fas_clear_rax(void) { return 0; }
FILE *fas_stdout(void) { return stdout; }
int fas_expected_sum(int n) { return n * (n + 1) + n / 2; }

int sum(int n, ...) {
  va_list args;
  double total = 0.0;
  va_start(args, n);
  for (int i = 0; i < n; ++i) {
    double value = va_arg(args, double);
    int addend = va_arg(args, int);
    total += value + (double)addend;
  }
  va_end(args);
  return (int)total;
}
