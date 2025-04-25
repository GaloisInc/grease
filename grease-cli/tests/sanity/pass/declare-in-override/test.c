/* Copyright (c) Galois, Inc. 2024 */

#include <stdint.h>

extern void* my_malloc(void);
extern void free(void* ptr);

void test(void) {
    int32_t* p = my_malloc();
    *p = 42;
    free(p);
}
// arm: ok()
// TODO: Why does PPC fail here?
// ppc32: check "PPCInvalidInstruction"
// x64: ok()
