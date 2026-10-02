#include <stddef.h>

static int block_shadow(size_t specialization_value, size_t runtime_value) {
  (void)specialization_value;
  {
    size_t N = runtime_value;
    {
      if (N == 3) return 1;
      return 2;
    }
  }
  return 0;
}

static int for_shadow(size_t specialization_value, size_t runtime_value) {
  (void)specialization_value;
  for (size_t N = runtime_value; N < runtime_value + 1; N += 1) {
    if (N == 3) return 1;
    return 2;
  }
  return 0;
}

static int switch_shadow(size_t specialization_value, size_t runtime_value) {
  (void)specialization_value;
  switch (runtime_value) {
    case 3: {
      size_t N = runtime_value;
      if (N == 3) return 1;
      return 2;
    }
    default:
      return 2;
  }
}

static int view_shadow(size_t specialization_value, size_t runtime_value) {
  (void)specialization_value;
  size_t values[1] = {runtime_value};
  size_t *N = &values[0];
  if (*N == 3) return 1;
  return 2;
}

static int index_shadow(size_t runtime_index) {
  int values[4] = {11, 22, 33, 44};
  size_t T = runtime_index;
  return values[T];
}

int main(void) {
  if (block_shadow(4, 3) != 1) return 1;
  if (for_shadow(4, 3) != 1) return 2;
  if (switch_shadow(4, 3) != 1) return 3;
  if (view_shadow(4, 3) != 1) return 4;
  if (index_shadow(1) != 22) return 5;
  return 0;
}
