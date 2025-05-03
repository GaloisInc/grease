/* Copyright (c) Galois, Inc. 2024 */

// Test multiple arguments

// all: flags {"--symbol", "test"}
// all: go(prog)

int inc(int x) { return x + 1; }

int test(int *x, int *y) { return inc(*x) + inc(*y); }
// all: ok()
