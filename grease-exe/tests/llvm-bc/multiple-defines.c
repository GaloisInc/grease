/* Copyright (c) Galois, Inc. 2024 */

// CFLAGS: $LLVM

/// flags {"--symbol", "test"}
/// go(prog)

#include <stdlib.h>

void panic(void) {
    abort();
}

int test(void) {
    panic();
    return 0;
}
/// must_fail()
/// check "Call to abort"
