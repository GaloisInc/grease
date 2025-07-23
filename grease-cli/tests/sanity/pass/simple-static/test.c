/* Copyright (c) Galois, Inc. 2025 */

// Simple load test that is compiled and loaded as a non-elf

int test(int *x) { return *x; }

// all: flags {"--entrypoint", "foo", "--use-debug-info-types", "--no-heuristics"}
// x64: go(prog)
// x64: ok()
