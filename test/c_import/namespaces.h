struct FasTagFunction { int value; };
int FasTagFunction(void);

struct FasTagGlobal { int value; };
extern int FasTagGlobal;

struct FasTagEnum { int value; };
enum { FasTagEnum = 7 };
