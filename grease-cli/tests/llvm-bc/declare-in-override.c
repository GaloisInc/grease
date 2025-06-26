/* Copyright (c) Galois, Inc. 2024 */

// CFLAGS: $LLVM

/// flags {"--symbol", "test"}
/// flags {"--overrides", "tests/llvm-bc/extra/my_malloc.llvm.cbl"}
/// go(prog)

#include <stdint.h>

extern void* my_malloc(void);
extern void free(void* ptr);

void test(void) {
    int32_t* p = my_malloc();
    *p = 42;
    free(p);
}
/// ok()
