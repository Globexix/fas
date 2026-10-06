void *plain_alloc(unsigned long n);
struct AllocHooks {
  void *(*field)(unsigned long) __attribute__((alloc_size(1)));
};
