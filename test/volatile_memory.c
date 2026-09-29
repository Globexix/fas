#include <stdbool.h>
#include <stdint.h>

static volatile uint32_t slot;

void fas_run(void);
bool fas_read_bool(void *p);
void fas_write_bool(void *p, bool value);
void *volatile_slot(void);
uint32_t observed_slot(void);

void *volatile_slot(void) { return (void *)&slot; }

uint32_t observed_slot(void) { return slot; }

int main(void) {
  fas_run();
  if (observed_slot() != 127)
    return 1;

  uint8_t byte = 0x80;
  if (!fas_read_bool(&byte))
    return 2;
  fas_write_bool(&byte, true);
  if (byte != 1)
    return 3;
  fas_write_bool(&byte, false);
  if (byte != 0)
    return 4;
  return 0;
}
