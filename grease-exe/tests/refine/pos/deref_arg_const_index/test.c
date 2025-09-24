/* Copyright (c) Galois, Inc. 2024 */

// Should be easy to deduce the precondition for this function

// all: flags {"--symbol", "test"}
// all: go(prog)

int test(int *ptr) { return ptr[8]; }
// all: ok()
