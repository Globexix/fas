extern void *(*f)(unsigned long);
void *wrap(void *(*g)(unsigned long) __attribute__((alloc_size(1))), unsigned long n);
