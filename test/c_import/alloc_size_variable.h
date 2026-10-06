void *plain_alloc(unsigned long n);
extern void *(*hooked_alloc)(unsigned long) __attribute__((alloc_size(1)));
void *marked_alloc(unsigned long n) __attribute__((alloc_size(1)));
