struct FasPoint { int x, y; };
struct FasPoint fas_point_named(int y, const char *name) __attribute__((nonnull(2)));
struct FasPoint fas_point_make(int x, int y);
int fas_point_sum(struct FasPoint point);
