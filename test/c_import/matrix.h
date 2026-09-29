typedef signed char fas_i8;
typedef fas_i8 fas_i8_chain;
typedef char fas_plain_char;
typedef unsigned char fas_u8;
typedef short fas_i16;
typedef unsigned short fas_u16;
typedef int fas_i32;
typedef unsigned int fas_u32;
typedef long fas_i64;
typedef unsigned long fas_u64;
typedef _Bool fas_bool;
typedef const int fas_const_i32;
typedef int *fas_int_pointer;
typedef int * const fas_const_int_pointer;
fas_i8 fas_i8_echo(fas_i8 value);
fas_plain_char fas_char_echo(fas_plain_char value);
fas_u8 fas_u8_echo(fas_u8 value);
fas_i16 fas_i16_echo(fas_i16 value);
fas_u16 fas_u16_echo(fas_u16 value);
fas_i32 fas_i32_echo(fas_i32 value);
fas_u32 fas_u32_echo(fas_u32 value);
fas_i64 fas_i64_echo(fas_i64 value);
fas_u64 fas_u64_echo(fas_u64 value);
fas_bool fas_bool_echo(fas_bool value);

enum FasEnum { FAS_ENUM_NEG = -3, FAS_ENUM_LARGE = 0xffffffffU };
int fas_enum_arg(enum FasEnum value);

struct FasRecord;
typedef struct FasRecord FasRecordAlias;
struct FasOtherRecord;
struct FasSameRecord;
typedef struct FasSameRecord FasSameRecord;
void *fas_void_pointer(void *value);
const char *fas_scalar_pointer(const char *value);
struct FasRecord *fas_record_pointer(struct FasRecord *value);
FasRecordAlias *fas_record_alias_pointer(FasRecordAlias *value);
struct FasOtherRecord *fas_other_pointer(struct FasOtherRecord *value);
struct FasSameRecord *fas_same_record(struct FasSameRecord *value);
int **fas_pointer_output(int **value);
int fas_variadic(int fixed, ...);

extern int fas_mutable_global;
extern const int fas_readonly_global;
extern const char *fas_mutable_pointer_global;
extern int * const fas_readonly_pointer_global;
extern fas_const_i32 fas_readonly_alias_global;
extern fas_int_pointer fas_mutable_typedef_pointer_global;
extern fas_const_int_pointer fas_readonly_typedef_pointer_global;

typedef struct FasRecord FasRecordAlias2;
typedef struct { int field; } FasAnonymous;
struct FasBitFields { unsigned int low : 3; unsigned int high : 5; };
extern struct FasBitFields fas_bitfield_global;
struct FasByValue { int field; };
struct FasByValue fas_struct_by_value(struct FasByValue value);
union FasUnionByValue { int integer; unsigned char byte; };
union FasUnionByValue fas_union_by_value(union FasUnionByValue value);
extern int fas_array_global[4];
float fas_float_value(float value);
double fas_double_value(double value);
long double fas_long_double_value(long double value);
__int128 fas_int128_value(__int128 value);
unsigned __int128 fas_uint128_value(unsigned __int128 value);
_BitInt(24) fas_bitint_value(_BitInt(24) value);
int (*fas_function_pointer)(int);
int fas_function_pointer_arg(int (*callback)(int));
int fas_function_pointer_nested(int (**callback)(int));
typedef int fas_vec4 __attribute__((vector_size(16)));
fas_vec4 fas_vector_value(fas_vec4 value);
int __attribute__((address_space(1))) *fas_address_space(
  int __attribute__((address_space(1))) *value);
int addr(int value);
int fas_nondefault_abi(int value) __attribute__((ms_abi));
static inline int fas_static_inline(int value) { return value; }

#define FAS_MACRO_ONLY 7
