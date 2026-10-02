#include "c_import/enum_types.h"

#include <stdio.h>

_Static_assert(_Generic((state_t)0, unsigned int: 1, default: 0), "state ABI type");
_Static_assert(_Generic(S_Y, int: 1, default: 0), "state constant type");
_Static_assert(_Generic((packed_t)0, unsigned char: 1, default: 0), "packed ABI type");
_Static_assert(_Generic(PACKED_MAX, int: 1, default: 0), "packed constant type");
_Static_assert(_Generic((wide_t)0, long: 1, default: 0), "wide ABI type");
_Static_assert(_Generic(WIDE_NEG, long: 1, default: 0), "wide negative constant type");
_Static_assert(_Generic(WIDE_LARGE, long: 1, default: 0), "wide constant type");
_Static_assert(_Generic((unsigned_enum_t)0, unsigned int: 1, default: 0), "unsigned enum ABI type");
_Static_assert(_Generic(UNSIGNED_MAX, unsigned int: 1, default: 0), "unsigned enum constant type");
_Static_assert(_Generic((FasTaggedEnumType)0, unsigned int: 1, default: 0), "tagged ABI type");
_Static_assert(_Generic(FAS_TAGGED_LAST, int: 1, default: 0), "tagged constant type");
_Static_assert(_Generic((FasEarlierEnumType)0, unsigned int: 1, default: 0), "earlier tagged ABI type");
_Static_assert(_Generic(FAS_EARLIER_LAST, int: 1, default: 0), "earlier tagged constant type");

int main(void) {
  const int constant_tab[2] = {S_X, S_Y};
  info_t tab[1] = {{am_b, S_Y}};
  printf("%d %d %d %d %u %d %ld %ld %u %d %d\n",
      tab[0].ammo, tab[0].st, constant_tab[0], constant_tab[1],
      (unsigned)c_packed_roundtrip(PACKED_MAX), c_enum_roundtrip(S_Y),
      c_wide_roundtrip(WIDE_NEG), c_wide_roundtrip(WIDE_LARGE),
      c_unsigned_roundtrip(UNSIGNED_MAX),
      c_tagged_enum_roundtrip(FAS_TAGGED_LAST),
      c_earlier_enum_roundtrip(FAS_EARLIER_LAST));
  return 0;
}
