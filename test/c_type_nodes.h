#include <stdbool.h>

typedef unsigned int NodeIntegerBase;
typedef NodeIntegerBase NodeIntegerAlias;
typedef _Bool NodeBoolBase;
typedef NodeBoolBase NodeBoolAlias;

typedef enum { NODE_ENUM_ZERO = 7, NODE_ENUM_VALUE = 11 } NodeEnumBase;
typedef NodeEnumBase NodeEnumAlias;
typedef enum __attribute__((packed)) {
  NODE_PACKED_ZERO = 0,
  NODE_PACKED_VALUE = 253
} NodePackedBase;
typedef NodePackedBase NodePackedAlias;
typedef enum { NODE_WIDE_ZERO = 0, NODE_WIDE_VALUE = 0x100000001ULL } NodeWideBase;
typedef NodeWideBase NodeWideAlias;

typedef int *NodePointerBase;
typedef NodePointerBase NodePointerAlias;
typedef int (*NodeFunctionBase)(int);
typedef NodeFunctionBase NodeFunctionAlias;
typedef NodeFunctionBase NodeFunctionArrayBase[2];
typedef NodeFunctionArrayBase NodeFunctionArrayAlias;
typedef int NodeArrayBase[3];
typedef NodeArrayBase NodeArrayAlias;
typedef int NodeIncompleteBase[];
typedef NodeIncompleteBase NodeIncompleteAlias;
typedef int NodeRowBase[3];
typedef NodeRowBase *NodeRowPointerBase;
typedef NodeRowPointerBase NodeRowPointerAlias;

typedef struct NodeRecord {
  int first;
  int second;
} NodeRecordBase;
typedef NodeRecordBase NodeRecordAlias;
typedef union NodeUnion {
  int number;
  unsigned int bits;
} NodeUnionBase;
typedef NodeUnionBase NodeUnionAlias;

typedef float NodeFloatBase;
typedef NodeFloatBase NodeFloatAlias;
typedef struct NodeFloatStorage {
  int before;
  NodeFloatAlias value;
  int after;
} NodeFloatStorageBase;
typedef NodeFloatStorageBase NodeFloatStorageAlias;

typedef const int NodeConstInteger;
typedef NodeConstInteger * volatile NodeVolatilePointer;
typedef NodeVolatilePointer * restrict NodeRestrictPointer;
typedef NodeRestrictPointer NodeQualifiedAlias;

extern NodeIntegerAlias node_integer;
extern NodeBoolAlias node_bool;
extern NodeEnumAlias node_enum;
extern NodePackedAlias node_packed;
extern NodeWideAlias node_wide;
extern NodePointerAlias node_pointer;
extern NodeFunctionAlias node_function;
extern NodeFunctionArrayAlias node_function_array;
extern NodeArrayAlias node_array;
extern NodeIncompleteAlias node_incomplete;
extern NodeRowPointerAlias node_row_pointer;
extern NodeRecordAlias node_record;
extern NodeUnionAlias node_union;
extern NodeFloatStorageAlias node_float_storage;
extern NodeQualifiedAlias node_qualified;

int node_pointer_value(NodePointerAlias value);
int node_function_value(NodeFunctionAlias function, int value);
int node_function_array_value(int index, int value);
int node_row_pointer_value(NodeRowPointerAlias value);
int node_qualified_value(NodeQualifiedAlias value);
