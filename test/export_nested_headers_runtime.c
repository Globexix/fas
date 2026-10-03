#include <stdint.h>
#include <stdio.h>
#include "export_nested_headers_z_types.h"
#include "export_nested_headers_a_nested.h"
int32_t accept_nested(ARecord *value);
int main(void) {
  if (accept_nested(NULL) != 1) return 1;
  puts("export_nested_headers: runtime ok");
  return 0;
}
