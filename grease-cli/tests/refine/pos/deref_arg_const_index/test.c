/* Copyright (c) Galois, Inc. 2024 */

// Should be easy to deduce the precondition for this function

// flags: --symbol test

int test(int *ptr) { return ptr[8]; }
// all: ok()
