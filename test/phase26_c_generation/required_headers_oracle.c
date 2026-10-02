#include "z_base.h"
#include "a_derived.h"

int main(void) {
  struct Base base = {3};
  struct Derived derived = {{4}, 5};
  return base.value == 3 && derived.base.value == 4 && derived.extra == 5 ? 0 : 1;
}
