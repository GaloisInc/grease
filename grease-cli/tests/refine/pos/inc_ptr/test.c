/* Copyright (c) Galois, Inc. 2024 */

int inc(int x) { return x + 1; }

int test(int *x) { return inc(*x); }
// all: ok()
