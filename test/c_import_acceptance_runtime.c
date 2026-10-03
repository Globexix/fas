#include "c_import/pointer_enum_macro_types.h"

struct PointerRecord pointer_record = {17};
union PointerUnion pointer_union = {23};
AnonymousRecordPointer anonymous_record_pointer = 0;
struct PointerCollision pointer_collision_object = {31};
PointerCollision pointer_collision_pointer = &pointer_collision_object;
struct AnonymousPointerFields anonymous_pointer_fields = {0, {0, 0}, 0, 37};
__typeof__(AnonymousEnumHigh) anonymous_enum_global = AnonymousEnumHigh;
struct AnonymousEnumFieldRecord anonymous_enum_field = {AnonymousFieldHigh, 9};

int pointer_record_value(PointerRecordPointer value) { return value->value; }
int pointer_union_value(PointerUnionPointer value) { return value->value; }
int anonymous_pointer_is_null(AnonymousRecordPointer value) { return value == 0; }
int pointer_collision_value(PointerCollision value) { return value->value; }
IncompletePointer incomplete_pointer(void) { return 0; }
size_t anonymous_pointer_fields_size(void) {
  return sizeof(struct AnonymousPointerFields);
}
size_t anonymous_pointer_tail_offset(void) {
  return offsetof(struct AnonymousPointerFields, tail);
}
int anonymous_enum_field_oracle(void) {
  return anonymous_enum_field.kind == AnonymousFieldHigh &&
         anonymous_enum_field.tail == 9;
}
size_t offset_float_values(void) {
  return offsetof(struct OffsetFloatStorage, values);
}
size_t offset_float_tail(void) {
  return offsetof(struct OffsetFloatStorage, tail);
}
