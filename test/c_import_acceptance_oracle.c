#include "c_import/pointer_enum_macro_types.h"

int main(void) {
  return pointer_record_value(&pointer_record) == 17 &&
                 pointer_union_value(&pointer_union) == 23 &&
                 anonymous_pointer_is_null(anonymous_record_pointer) &&
                 pointer_collision_value(pointer_collision_pointer) == 31 &&
                 incomplete_pointer() == 0 &&
                 anonymous_pointer_fields.pointer == 0 &&
                 anonymous_pointer_fields.pointers[0] == 0 &&
                 anonymous_pointer_fields.union_pointer == 0 &&
                 anonymous_pointer_fields_size() == sizeof(anonymous_pointer_fields) &&
                 anonymous_pointer_tail_offset() ==
                     offsetof(struct AnonymousPointerFields, tail) &&
                 read_anonymous_parameter(anonymous_parameter_address()) == 41 &&
                 anonymous_enum_global == AnonymousEnumHigh &&
                 sizeof(anonymous_enum_global) == sizeof(int) &&
                 anonymous_enum_field_oracle() &&
                 offset_float_values() == offsetof(struct OffsetFloatStorage, values) &&
                 offset_float_tail() == offsetof(struct OffsetFloatStorage, tail) &&
                 SIZE_T_MACRO == sizeof(int) && PTRDIFF_MACRO == -5 &&
                 UINTPTR_MACRO == 7 && INTPTR_MACRO == -6 &&
                 CUSTOM_SIZE_MACRO == sizeof(int) && SIZE_MAX == (size_t)-1
             ? 0
             : 1;
}
