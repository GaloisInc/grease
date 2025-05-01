/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

int inc(int x) { return x + 1; }

int test(int *x) { return inc(*x); }
// all: ok()
