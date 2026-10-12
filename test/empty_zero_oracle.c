#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef float f32x3 __attribute__((ext_vector_type(3)));
typedef double f64x5 __attribute__((ext_vector_type(5)));
typedef uint32_t u32x3 __attribute__((ext_vector_type(3)));
struct Local { uint8_t tag; f32x3 floats; f64x5 doubles; u32x3 integers; };
struct Nested { f32x3 values[2]; struct Local inner; };
struct Tail { uint8_t c; f32x3 w[2]; };
struct Padding { uint8_t b; uint64_t w; };
struct Node { uint8_t tag; struct Padding inner; };
struct Big { uint8_t k; uint64_t p[20]; };
union FasZeroUnion { uint64_t word; uint8_t bytes[16]; };

static void dump(const char *label, const void *object, size_t size) {
  const uint8_t *bytes = object;
  printf("%s", label);
  for (size_t i = 0; i < size; ++i) printf("%02x", bytes[i]);
  putchar('\n');
}

static void dump_zero(const char *label, size_t size) {
  uint8_t bytes[512] = {0};
  dump(label, bytes, size);
}

int main(void) {
  puts("dirty ab");
  dump_zero("local ", sizeof(struct Local));
  dump_zero("vectors ", sizeof(f32x3[2]));
  dump_zero("nested ", sizeof(struct Nested));
  dump_zero("partial ", sizeof(struct Tail));
  dump_zero("nested elements ", sizeof(struct Tail[3]));
  dump_zero("union ", sizeof(union FasZeroUnion));
  struct Node nodes[20];
  memset(nodes, 0, sizeof(nodes));
  for (size_t i = 0; i < 20; ++i) nodes[i].tag = 1;
  for (size_t i = 0; i < 20; ++i)
    dump("nested constant ", &nodes[i].inner, sizeof(struct Padding));
  struct Big large;
  memset(&large, 0, sizeof(large));
  large.k = 1;
  for (size_t i = 0; i < 20; ++i) large.p[i] = i + 1;
  dump("designated constant ", &large, sizeof(struct Big));
  return 0;
}
