/* Copyright (c) Galois, Inc. 2024 */

// gitlab#38

// all: flags {"--symbol", "test"}
// all: go(prog)

int inc(int x) { return x + 1; }
int test(int **x) { return inc(**x); }
// all: ok()
