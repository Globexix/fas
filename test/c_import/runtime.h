enum RuntimeKind { RUNTIME_KIND = 27 };

struct RuntimeOpaque;
typedef struct RuntimeOpaque RuntimeOpaque;

extern int runtime_global;
int runtime_global_increment(int amount);
enum RuntimeKind runtime_enum_echo(enum RuntimeKind value);
struct RuntimeOpaque *runtime_opaque_value(void);
struct RuntimeOpaque *runtime_opaque_roundtrip(struct RuntimeOpaque *value);
void runtime_pointer_out(int **out);
