/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test
// flags: --req in-text

__attribute__((section(".data"))) int inc(int x) { return x + 1; }

int test(int x) { return inc(x); }
// all: req_failed()
// all: check ".text"
