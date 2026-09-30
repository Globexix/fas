#include "arrays.h"
char names[4][8];
const int readonly_values[4] = {3, 11, 17, 23};
int container_values[2][3];
void fill_names(void) {
  for (int row = 0; row < 4; ++row)
    for (int column = 0; column < 8; ++column)
      names[row][column] = row * 8 + column;
}
int inspect_names(void) { return names[1][6]; }
int read_container(void) { return container_values[1][2]; }
