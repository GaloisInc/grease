#include <stddef.h>
extern int *foo(void);
int *bar(void) { return foo(); }
// note: intentional name collision with libc
void *malloc(size_t n) { (void)n; return (void *)0; }
