/* Copyright (c) Galois, Inc. 2025 */

// Simple load test that is compiled and loaded as a non-elf

int test(int *x) { return *x; }

// all: flags {"--load-base", "0x1000", "--address", "0x1000", "--raw-binary"}
// arm: go(prog)
// arm: ok()
