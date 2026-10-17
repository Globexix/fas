#include <stdint.h>

struct NestedOuter {
  struct NestedInner {
    int32_t x;
    int16_t y;
  } pos;
  int32_t tag;
};

struct NestedDeep {
  struct NestedMid {
    struct NestedLeaf {
      uint8_t a;
      uint32_t b;
    } leaf;
    int16_t c;
  } mid;
  uint8_t d;
};

union NestedUnion {
  struct NestedPair {
    int16_t a;
    int16_t b;
  } pair;
  int32_t word;
};

struct NestedAnon {
  union {
    struct NestedInAnon {
      uint32_t lo;
      uint32_t hi;
    } parts;
    uint64_t whole;
  };
  int32_t after;
};

struct NestedFwd;
struct NestedHolder {
  struct NestedFwd *link;
  int32_t id;
  struct NestedFwd {
    int32_t v;
  } body;
};
