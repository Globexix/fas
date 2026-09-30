struct FasTagFunction { int value; };
int FasTagFunction(void);

struct FasTagGlobal { int value; };
extern int FasTagGlobal;

struct FasTagEnum { int value; };
enum { FasTagEnum = 7 };

struct FasTagTypedef { int value; };
typedef unsigned int FasTagTypedef;
struct FasTagTypedef *fas_tag_typedef_pointer(void);
