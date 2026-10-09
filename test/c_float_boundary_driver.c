#include "api.h"
#include "c_float_boundary.h"
#include <stdio.h>

int main(void) {
  if (fas_float_state.narrow != 1.25f || fas_float_state.wide != 3.5) return 1;
  if (fas_scale_f32(1.5f) != 3.0f) return 2;
  if (fas_scale_f64(1.25) != 3.75) return 3;
  if (fas_import_probe() != 0) return 4;
  if (c_global_f32 != 1.75f || c_global_f64 != 5.0) return 5;
  if (c_float_state.narrow != 1.75f || c_float_state.wide != 5.0) return 6;
  double result = fas_mixed(9, 8, 7, 6, 5, 4, 3, 0.25, 1.5, 2.25, 3.125,
                            4.0625, 5.03125, 6.015625, 7.0078125, 8.00390625);
  printf("%.17g %a\n", result, result);
  return 0;
}
