#include "c_float_boundary.h"
#include <stdio.h>

int main(void) {
  double wide = c_double_echo(2.5);
  printf("%.17g %a\n", wide, wide);
  double imported = c_mixed(1, 2, 3, 4, 5, 6, 7, 0.5, 1.25, 2.5, 3.75,
                            4.5, 5.25, 6.125, 7.0625, 8.03125);
  printf("%.17g %a\n", imported, imported);
  double exported = c_mixed(9, 8, 7, 6, 5, 4, 3, 0.25, 1.5, 2.25, 3.125,
                            4.0625, 5.03125, 6.015625, 7.0078125, 8.00390625);
  printf("%.17g %a\n", exported, exported);
  return 0;
}
