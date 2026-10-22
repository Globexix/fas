#include "c_record_values.h"
#include <stdio.h>

int32_t fas_record_probe(void) {
  RvSmall small = {3, -7, 100000};
  RvSmall small2 = rv_small(5, small);
  printf("small %d %d %d\n", small2.a, small2.b, small2.c);
  RvMixed mixed = {41, 1.5f, 0.125};
  RvMixed mixed2 = rv_mixed(mixed, 3.0);
  printf("mixed %d %.9g %.17g\n", mixed2.i, mixed2.f, mixed2.d);
  RvF2 f2[2] = {{1.0f, 2.0f}, {0.5f, -3.0f}};
  f2[0] = rv_f2(f2[0], 1.5f, f2[1]);
  printf("f2 %.9g %.9g\n", f2[0].x, f2[0].y);
  RvD2 d = {1.0, 2.0}, e = {0.5, 0.25}, f = {3.0, 7.0}, g = {-1.0, 4.0}, h = {2.0, -6.0};
  RvD2 d2 = rv_d2(d, e, f, g, h);
  printf("d2 %.17g %.17g\n", d2.x, d2.y);
  RvBig big = {1, 2, 3, 9.0};
  big = rv_big(big, 11);
  printf("big %lld %lld %lld %.17g\n", (long long)big.a, (long long)big.b,
         (long long)big.c, big.d);
  RvUnion u;
  u.i = 0x1234;
  RvUnion u2 = rv_union(u);
  printf("union %lld\n", (long long)u2.i);
  RvPacked p = {7, -9};
  RvPacked p2 = rv_packed(p);
  printf("packed %d %d\n", p2.c, p2.i);
  RvAligned a = {6};
  RvAligned a2 = rv_aligned(4, a);
  printf("aligned %d\n", a2.x);
  printf("sum %lld\n", (long long)rv_sum(small, mixed, f2[1], d, big, p));
  RvF2 made = rv_make();
  printf("make %.9g %.9g\n", made.x, made.y);
  RvD2 s = rv_static(d, made);
  printf("static %.17g %.17g\n", s.x, s.y);
  return 0;
}
