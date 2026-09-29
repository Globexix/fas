#include "runtime.h"

struct RuntimeOpaque {
  int value;
};

int runtime_global = 3;
static int pointer_value = 41;
static struct RuntimeOpaque opaque_value = { 73 };

int runtime_global_increment(int amount) {
  runtime_global += amount;
  return runtime_global;
}

enum RuntimeKind runtime_enum_echo(enum RuntimeKind value) {
  return value;
}

struct RuntimeOpaque *runtime_opaque_value(void) {
  return &opaque_value;
}

struct RuntimeOpaque *runtime_opaque_roundtrip(struct RuntimeOpaque *value) {
  return value;
}

void runtime_pointer_out(int **out) {
  *out = &pointer_value;
}
