#include <stdint.h>

typedef const char *fas_c_string;
typedef char fas_plain_char;
typedef fas_plain_char *fas_plain_char_pointer;
typedef fas_plain_char_pointer fas_plain_string;
typedef signed char fas_signed_char;
typedef unsigned char fas_unsigned_char;

void fas_take_char(const char *value);
void fas_take_array(const char value[]);
void fas_take_sized_array(const char value[4]);
void fas_take_unnamed(const char *);
void fas_take_alias(fas_c_string value);
void fas_take_plain_char(fas_plain_char *value);
void fas_take_plain_string(fas_plain_string value);
void fas_take_char_pointer_pointer(char **value);
void fas_take_signed(signed char *value);
void fas_take_unsigned(unsigned char *value);
void fas_take_signed_alias(fas_signed_char *value);
void fas_take_unsigned_alias(fas_unsigned_char *value);
void fas_take_u8(uint8_t *value);
