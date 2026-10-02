#include "c_type_nodes.h"

static int node_first_function(int value) { return value + 3; }
static int node_second_function(int value) { return value + 17; }
static int node_values[3] = {19, 23, 29};
static int node_row[3] = {31, 37, 43};
static const int node_qualified_value_storage = 71;
static const int * volatile node_qualified_middle = &node_qualified_value_storage;

NodeIntegerAlias node_integer = 4242;
NodeBoolAlias node_bool = true;
NodeEnumAlias node_enum = NODE_ENUM_VALUE;
NodePackedAlias node_packed = NODE_PACKED_VALUE;
NodeWideAlias node_wide = NODE_WIDE_VALUE;
NodePointerAlias node_pointer = &node_values[1];
NodeFunctionAlias node_function = node_first_function;
NodeFunctionArrayAlias node_function_array = {
    node_first_function, node_second_function};
NodeArrayAlias node_array = {19, 23, 29};
NodeRowPointerAlias node_row_pointer = &node_row;
NodeRecordAlias node_record = {59, 61};
NodeUnionAlias node_union = {.number = 67};
NodeFloatStorageAlias node_float_storage = {73, 3.5f, 79};
NodeQualifiedAlias node_qualified = &node_qualified_middle;

int node_pointer_value(NodePointerAlias value) { return *value; }
int node_function_value(NodeFunctionAlias function, int value) {
  return function(value);
}
int node_function_array_value(int index, int value) {
  return node_function_array[index](value);
}
int node_row_pointer_value(NodeRowPointerAlias value) { return (*value)[2]; }
int node_qualified_value(NodeQualifiedAlias value) { return **value; }
