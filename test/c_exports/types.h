#ifndef FAS_EXPORT_TYPES_H
#define FAS_EXPORT_TYPES_H
struct ExportTag { int value; };
typedef struct { int value; } ExportAlias;
static inline int export_inline(int value) { return value + 5; }
#endif
