/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

int test(int *ptr) { return ptr == 0; }
// all: ok()
