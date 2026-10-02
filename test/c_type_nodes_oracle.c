#include "c_type_nodes.h"
#include <stdio.h>

NodeIncompleteAlias node_incomplete = {41, 47, 53};

int main(void) {
  if (node_integer != 4242 || !node_bool) return 1;
  if (node_enum != NODE_ENUM_VALUE || node_packed != NODE_PACKED_VALUE) return 2;
  if (node_wide != NODE_WIDE_VALUE) return 3;
  if (node_pointer_value(node_pointer) != 23) return 4;
  if (node_function_value(node_function, 5) != 8) return 5;
  if (node_function_array_value(1, 5) != 22) return 6;
  if (node_array[1] != 23 || node_incomplete[1] != 47) return 7;
  if (node_row_pointer_value(node_row_pointer) != 43) return 8;
  if (node_record.first != 59 || node_record.second != 61) return 9;
  if (node_union.number != 67) return 10;
  if (sizeof(node_float_storage) != 12 || node_float_storage.before != 73
      || node_float_storage.after != 79)
    return 11;
  if (node_qualified_value(node_qualified) != 71) return 12;
  printf("%d\n", node_integer + node_bool + node_enum + node_packed
                      + (int)node_wide + node_pointer_value(node_pointer)
                      + node_function_value(node_function, 5)
                      + node_function_array_value(1, 5) + node_array[1]
                      + node_incomplete[1] + node_row_pointer_value(node_row_pointer)
                      + node_record.first + node_record.second + node_union.number
                      + node_float_storage.before + node_float_storage.after
                      + node_qualified_value(node_qualified));
  return 0;
}
