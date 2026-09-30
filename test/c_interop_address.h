static inline int fas_interop_static_helper(int value) {
  return value + 1;
}

static inline int fas_interop_address_only(int value) {
  return value * 2;
}

static int fas_interop_raw_target(int value) { return value + 11; }

static int (*fas_interop_raw_get(void))(int) {
  return fas_interop_raw_target;
}

static int fas_interop_raw_apply(int (*callback)(int), int value) {
  return callback(value);
}

static int fas_interop_raw_array(int (*values)[4]) { return (*values)[2]; }

int (*fas_interop_raw_global)(int) = fas_interop_raw_target;

struct FasInteropRawCallbackRecord { int (*callback)(int); };
