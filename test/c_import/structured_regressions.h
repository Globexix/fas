typedef int *restrict fas_restrict_pointer;
void fas_restrict_parameter(int *restrict value);
void fas_restrict_nested(int *restrict *value);
void fas_restrict_typedef(fas_restrict_pointer value);
typedef long fas_read_function_t(void *, char *, unsigned long);
typedef long fas_write_function_t(void *, const char *, unsigned long);
typedef _Complex double fas_complex_double;
