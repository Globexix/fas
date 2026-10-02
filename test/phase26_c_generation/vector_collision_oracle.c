#include <stdint.h>

typedef int32_t c_vector2 __attribute__((ext_vector_type(2)));

struct CVectorName {
  int32_t value;
};

struct Holder {
  c_vector2 lanes;
};

static struct Holder holder = {{1, 2}};
static struct CVectorName collision = {3};

int main(void) {
  return collision.value == 3 && holder.lanes[1] == 2 ? 0 : 1;
}
