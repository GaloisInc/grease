/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: flags {"--req", "in-text"}
// all: go(prog)

int __attribute__((noinline)) inc(int x) { return x + 1; }

int test(int x) { return inc(x); }
// all: ok()
