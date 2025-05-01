/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

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
