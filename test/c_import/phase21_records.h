#include <stdlib.h>
struct FasTagRecord { int first; unsigned char second; };
typedef struct { unsigned char byte; int word; } FasAnonymousRecord;
struct FasAliasRecord { int value; };
typedef struct FasAliasRecord FasAlias;
struct FasInnerRecord { short left; int right; };
struct FasNestedRecord { struct FasInnerRecord inner; int values[2]; };
struct FasSelfRecord { struct FasSelfRecord *next; int value; };
struct __attribute__((aligned(16))) FasAlignedRecord { int value; };
struct __attribute__((packed)) FasPackedRecord { unsigned char byte; int word; };
union FasUnionRecord { int value; unsigned char byte; };
void fas_union_pointer(union FasUnionRecord *value);
union FasUnionRecord *fas_union_pointer_result(void);
int fas_union_by_value(union FasUnionRecord value);
union FasFloatUnion { float value; unsigned int bits; };
union FasBitfieldUnion { unsigned int value : 3; unsigned int word; };
struct FasBitfieldRecord { unsigned int value : 3; };
struct FasFlexibleRecord { int length; unsigned char data[]; };
struct FasAnonymousMemberRecord { union { int integer; unsigned char byte; }; };
struct FasAnonymousNestedRecord {
  struct {
    int outer;
    union { unsigned int nested; unsigned char raw[4]; };
  };
  unsigned int tail;
};
union FasAnonymousStructUnion {
  struct { unsigned short low; unsigned short high; };
  unsigned int word;
};
struct FasAnonymousCollisionRecord {
  union { int first; };
  union { int second; };
};
struct FasAnonymousUnsupportedRecord {
  union __attribute__((transparent_union)) { int value; };
};
struct FasFloatRecord { int before; float value; int after; };
struct FasNestedFloatRecord { int before; struct FasFloatRecord inner; int after; };
struct FasFloatArrayRecord { int before; double data[6]; int after; };
typedef int (*FasCallback)(int);
struct FasFunctionPointerRecord { int (*callback)(int); };
extern FasCallback fas_callback_global;
FasCallback fas_callback_result(void);
int fas_callback_parameter(FasCallback callback);
struct FasFieldNamesRecord { int handle; int len; int view; int i32; };
struct FasKeywordFieldRecord { int opaque; int fn; int var; };
struct FasConstFieldRecord { const int value; };
struct FasNestedConstFieldRecord { struct FasConstFieldRecord inner; };
struct FasConstPointerRecord { const char *value; };
extern struct FasSelfRecord fas_address_self;
extern struct FasNestedRecord fas_address_nested;
extern struct FasSelfRecord fas_address_self_array[2];
