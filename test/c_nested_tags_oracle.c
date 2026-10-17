#include "c_nested_tags.h"
#include <stddef.h>
#include <stdio.h>

static void report_sizes(void) {
  printf("sizes %zu %zu %zu %zu %zu %zu %zu %zu %zu %zu %zu %zu\n",
         sizeof(struct NestedOuter), offsetof(struct NestedOuter, tag),
         sizeof(struct NestedDeep), offsetof(struct NestedDeep, d),
         sizeof(union NestedUnion), sizeof(struct NestedAnon),
         offsetof(struct NestedAnon, after), sizeof(struct NestedHolder),
         sizeof(struct NestedInner), sizeof(struct NestedLeaf),
         sizeof(struct NestedInAnon), sizeof(struct NestedFwd));
}

static void report_outer(const struct NestedOuter *value) {
  printf("outer %d %d %d\n", value->pos.x, (int)value->pos.y, value->tag);
}

static void report_deep(const struct NestedDeep *value) {
  printf("deep %u %u %d %u\n", (unsigned)value->mid.leaf.a, value->mid.leaf.b,
         (int)value->mid.c, (unsigned)value->d);
}

static void report_union(const union NestedUnion *value) {
  printf("union %d %d %d\n", (int)value->pair.a, (int)value->pair.b, value->word);
}

static void report_anon(const struct NestedAnon *value) {
  printf("anon %u %u %llu %d\n", value->parts.lo, value->parts.hi,
         (unsigned long long)value->whole, value->after);
}

static void report_holder(const struct NestedHolder *value) {
  printf("holder %d %d\n", value->body.v, value->id);
}

int main(void) {
  struct NestedOuter outer = {{0, 0}, 0};
  struct NestedDeep deep = {{{0, 0}, 0}, 0};
  union NestedUnion word = {0};
  struct NestedAnon anon = {0};
  struct NestedHolder holder = {0};

  report_sizes();

  outer.pos.x = -5;
  outer.pos.y = 300;
  outer.tag = 77;
  report_outer(&outer);
  outer.pos.x = outer.pos.x + 2;
  outer.tag = outer.tag + outer.pos.y;
  report_outer(&outer);

  deep.mid.leaf.a = 9;
  deep.mid.leaf.b = 0x01020304u;
  deep.mid.c = -7;
  deep.d = 200;
  report_deep(&deep);
  deep.mid.leaf.b = deep.mid.leaf.b + 1;
  report_deep(&deep);

  word.word = 0x0005FFFE;
  report_union(&word);
  word.pair.b = -1;
  report_union(&word);

  anon.parts.lo = 0x11223344u;
  anon.parts.hi = 0x55667788u;
  anon.after = 42;
  report_anon(&anon);
  anon.parts.hi = 1;
  report_anon(&anon);

  holder.body.v = 5;
  holder.id = 8;
  report_holder(&holder);
  holder.body.v = holder.body.v * 3;
  report_holder(&holder);
  return 0;
}
