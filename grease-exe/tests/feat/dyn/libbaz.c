#include <stddef.h>
// note: intentional name collision with libfoo.c
int *foo(void) { return (int *)0; }
// note: intentional name collision with libc
void *malloc(size_t n) { (void)n; return (void *)0; }
