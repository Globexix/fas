#ifndef FAS_C_FLOAT_BOUNDARY_H
#define FAS_C_FLOAT_BOUNDARY_H

typedef struct CFloatState {
  float narrow;
  double wide;
} CFloatState;

extern CFloatState c_float_state;
extern float c_global_f32;
extern double c_global_f64;

float c_float_echo(float value);
double c_double_echo(double value);
double c_mixed(int i0, int i1, int i2, int i3, int i4, int i5, int i6,
               double d0, double d1, double d2, double d3, double d4,
               double d5, double d6, double d7, double d8);

#endif
