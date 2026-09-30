#include <errno.h>
#include <stdio.h>
enum { FAS_MACRO_ENUM_BASE = 37 };
#define FAS_MACRO_UHEX 0xdeadbeefu
#define FAS_MACRO_LONG 19L
#define FAS_MACRO_ENUM FAS_MACRO_ENUM_BASE
#define FAS_MACRO_ALIAS 41
#define FAS_MACRO_CHAIN FAS_MACRO_ALIAS
#define FAS_MACRO_STRING "fas"
#define FAS_MACRO_FUNCTION(x) ((x) + 1)
#define FAS_MACRO_ERRNO errno
#define FAS_MACRO_POINTER ((void *)0)
#define FAS_MACRO_EMPTY
#define addr 7
