/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: flags {"--req", "in-text"}
// all: go(prog)

__attribute__((section(".data"))) int inc(int x) { return x + 1; }

int test(int x) { return inc(x); }
// all: req_failed()
// all: check ".text"
