/* Copyright (c) Galois, Inc. 2024 */

// Test multiple arguments

// flags: --symbol test

int inc(int x) { return x + 1; }

int test(int *x, int *y) { return inc(*x) + inc(*y); }
// all: ok()
