#include <stdint.h>

typedef const char *fas_c_string;

void fas_take_char(const char *value);
void fas_take_unnamed(const char *);
void fas_take_alias(fas_c_string value);
void fas_take_signed(signed char *value);
void fas_take_unsigned(unsigned char *value);
void fas_take_u8(uint8_t *value);
