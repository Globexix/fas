void *plain_alloc(unsigned long n);
typedef void *(*alloc_fn)(unsigned long) __attribute__((alloc_size(1)));
