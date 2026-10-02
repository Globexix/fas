static int fas_raw_target(int value) { return value + 11; }
static int (*fas_raw_get(void))(int) { return fas_raw_target; }
static int fas_raw_apply(int (*callback)(int), int value) {
  return callback(value);
}
static int fas_raw_array(int (*values)[4]) { return (*values)[2]; }
int (*fas_raw_global)(int) = fas_raw_target;
struct FasRawCallbackRecord { int (*callback)(int); };
