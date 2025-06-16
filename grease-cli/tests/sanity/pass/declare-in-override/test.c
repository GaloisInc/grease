/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// arm: flags {"--overrides", "tests/sanity/pass/declare-in-override/my_malloc.aux.armv7l.cbl"}
// ppc32: flags {"--overrides", "tests/sanity/pass/declare-in-override/my_malloc.aux.ppc32.cbl"}
// x64: flags {"--overrides", "tests/sanity/pass/declare-in-override/my_malloc.aux.x86_64.cbl"}
// all: go(prog)

#include <stdint.h>
#include <stddef.h>

void* my_malloc(void) {
    return NULL;
}

void free(void* ptr) {
    return;
}

void test(void) {
    int32_t* p = my_malloc();
    *p = 42;
    free(p);
}
// arm: ok()
// TODO: Why does PPC fail here?
// ppc32: check "PPCInvalidInstruction"
// x64: ok()
