#if defined(__x86_64__) && ASSEMBLY_TABLE_VALUE == 42
.section .rodata
shared_table:
.long ASSEMBLY_TABLE_VALUE
.text
.globl table_first
.type table_first,@function
table_first:
jmp shared_read
.globl table_second
.type table_second,@function
table_second:
jmp shared_read
shared_read:
movl shared_table(%rip), %eax
ret
.section .note.GNU-stack,"",@progbits
#else
#error assembly preprocessing flags missing
#endif
