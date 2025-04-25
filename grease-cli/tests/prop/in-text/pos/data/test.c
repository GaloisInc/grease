/* Copyright (c) Galois, Inc. 2024 */

__attribute__((section(".data"))) int inc(int x) { return x + 1; }

int test(int x) { return inc(x); }
// all: req_failed()
// all: check ".text"
