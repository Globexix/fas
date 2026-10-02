typedef unsigned long FasOveraligned __attribute__((aligned(16)));
typedef unsigned int FasAlignmentEqualsSize __attribute__((aligned(4)));
extern FasOveraligned fas_overaligned_global;
FasOveraligned fas_overaligned_parameter(FasOveraligned value);
