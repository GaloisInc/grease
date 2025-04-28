/* Copyright (c) Galois, Inc. 2024 */

// flags(arm): --overrides tests/sanity/pass/declare-in-override/my_malloc.aux.armv7l.cbl
// flags(ppc32): --overrides tests/sanity/pass/declare-in-override/my_malloc.aux.ppc32.cbl
// flags(x64): --overrides tests/sanity/pass/declare-in-override/my_malloc.aux.x86_64.cbl

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
