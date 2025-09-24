/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int test(int *ptr) { return ptr == 0; }
// all: ok()
