/* Copyright (c) Galois, Inc. 2025 */

// Simple load test that is compiled and loaded as a non-elf

int test(int *x) { return *x; }

// all: flags {"--address", "0", "--raw-binary"}
// arm: go(prog)
// arm: ok()
