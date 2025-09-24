/* Copyright (c) Galois, Inc. 2024 */

// Should behave the same as just having a pointer parameter

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stdint.h>
int test(uintptr_t ptr) { return *((int *)(void *)ptr); }
// all: ok()
