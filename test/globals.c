#include <stdint.h>

extern uint32_t Exported;
extern uint32_t Imported;

uint32_t Imported = 5;

uint32_t fas_zero_check(void);
uint32_t fas_address_bump(void);
uint32_t fas_compound_update(void);
uint32_t fas_imported_update(void);
uint32_t fas_exported_observe(void);
uint32_t fas_array_struct(void);
uint32_t fas_generic_update(void);
uint32_t fas_volatile_roundtrip(void);

uint32_t bump(void *pointer) {
  uint32_t *value = pointer;
  *value += 2;
  return *value;
}

void mutate_exported(void) { Exported += 10; }

int main(void) {
  if (fas_zero_check() != 0)
    return 1;
  if (fas_address_bump() != 2)
    return 2;
  if (fas_compound_update() != 5)
    return 3;
  if (fas_imported_update() != 12 || Imported != 12)
    return 4;
  if (fas_exported_observe() != 10 || Exported != 13)
    return 5;
  if (fas_array_struct() != 36)
    return 6;
  if (fas_generic_update() != 7)
    return 7;
  if (fas_volatile_roundtrip() != 27)
    return 8;
  return 0;
}
