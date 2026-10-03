#include <stddef.h>
#include <stdint.h>

struct PointerRecord {
  int value;
};
typedef struct PointerRecord *PointerRecordPointer;
extern struct PointerRecord pointer_record;
int pointer_record_value(PointerRecordPointer value);

union PointerUnion {
  int value;
};
typedef union PointerUnion *PointerUnionPointer;
extern union PointerUnion pointer_union;
int pointer_union_value(PointerUnionPointer value);

typedef struct {
  int value;
} *AnonymousRecordPointer;
extern AnonymousRecordPointer anonymous_record_pointer;
int anonymous_pointer_is_null(AnonymousRecordPointer value);

struct PointerCollision {
  int value;
};
typedef struct PointerCollision *PointerCollision;
extern PointerCollision pointer_collision_pointer;
int pointer_collision_value(PointerCollision value);

struct IncompletePointerTarget;
typedef struct IncompletePointerTarget *IncompletePointer;
IncompletePointer incomplete_pointer(void);

struct AnonymousPointerFields {
  struct {
    int value;
  } *pointer;
  struct {
    int value;
  } *pointers[2];
  union {
    int value;
  } *union_pointer;
  uint32_t tail;
};
extern struct AnonymousPointerFields anonymous_pointer_fields;
size_t anonymous_pointer_fields_size(void);
size_t anonymous_pointer_tail_offset(void);

static inline int read_anonymous_parameter(struct { int value; } *value) {
  return value->value;
}
static inline void *anonymous_parameter_address(void) {
  static struct { int value; } value = {41};
  return &value;
}

enum { AnonymousEnumZero = 0, AnonymousEnumHigh = 42 };
extern __typeof__(AnonymousEnumHigh) anonymous_enum_global;
struct AnonymousEnumFieldRecord {
  enum { AnonymousFieldLow = 1, AnonymousFieldHigh = 7 } kind;
  int tail;
};
extern struct AnonymousEnumFieldRecord anonymous_enum_field;
int anonymous_enum_field_oracle(void);

struct OffsetFloatStorage {
  uint8_t head;
  float values[3];
  int tail;
};
size_t offset_float_values(void);
size_t offset_float_tail(void);

#define SIZE_T_MACRO ((size_t)sizeof(int))
#define PTRDIFF_MACRO ((ptrdiff_t)-5)
#define UINTPTR_MACRO ((uintptr_t)7)
#define INTPTR_MACRO ((intptr_t)-6)
typedef size_t CustomSizeAlias;
#define CUSTOM_SIZE_MACRO ((CustomSizeAlias)sizeof(int))
