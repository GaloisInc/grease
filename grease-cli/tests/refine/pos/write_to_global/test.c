/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#202.

// flags: --symbol test

int glob[128] = {0};

void test(void) {
    glob[0] = 42;
}
// all: ok()
