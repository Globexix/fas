#include "arrays.h"
extern int container_values[2][3];
int read_container(void);
int main(void) {
  fill_names();
  if (names[3][7] != 31) return 1;
  if (readonly_values[2] != 17) return 2;
  names[1][6] = 99;
  container_values[1][2] = 73;
  if (inspect_names() != 99) return 3;
  if (read_container() != 73) return 4;
  return 0;
}
