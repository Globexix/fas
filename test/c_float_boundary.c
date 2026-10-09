#include "c_float_boundary.h"

CFloatState c_float_state;
float c_global_f32;
double c_global_f64;

float c_float_echo(float value) { return value + 0.5f; }
double c_double_echo(double value) { return value * 2.0; }

double c_mixed(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
               double d0, double d1, double d2, double d3, double d4,
               double d5, double d6, double d7, double d8) {
  return i0 + 2 * i1 + 4 * i2 + 8 * i3 + 16 * i4 + 32 * i5 + 64 * i6 +
         d0 + 2.0 * d1 + 4.0 * d2 + 8.0 * d3 + 16.0 * d4 + 32.0 * d5 +
         64.0 * d6 + 128.0 * d7 + 256.0 * d8;
}
