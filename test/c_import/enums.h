enum FasImplicitValues {
  FAS_IMPLICIT_FIRST,
  FAS_IMPLICIT_EXPLICIT = 7,
  FAS_IMPLICIT_AFTER_EXPLICIT,
  FAS_IMPLICIT_NEGATIVE = -5,
  FAS_IMPLICIT_AFTER_NEGATIVE,
  FAS_IMPLICIT_NEAR_MAX = 2147483646,
  FAS_IMPLICIT_INT_MAX
};

_Static_assert(FAS_IMPLICIT_FIRST == 0, "first implicit value");
_Static_assert(FAS_IMPLICIT_EXPLICIT == 7, "explicit value");
_Static_assert(FAS_IMPLICIT_AFTER_EXPLICIT == 8, "implicit after explicit");
_Static_assert(FAS_IMPLICIT_NEGATIVE == -5, "negative explicit value");
_Static_assert(FAS_IMPLICIT_AFTER_NEGATIVE == -4, "implicit after negative");
_Static_assert(FAS_IMPLICIT_NEAR_MAX == 2147483646, "near INT_MAX");
_Static_assert(FAS_IMPLICIT_INT_MAX == 2147483647, "implicit INT_MAX");
