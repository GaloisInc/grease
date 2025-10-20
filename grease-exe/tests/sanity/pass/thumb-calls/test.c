/* Copyright (c) Galois, Inc. 2025 */

// Simple call test to make sure calling thumb mode functions works.

__attribute__((target("thumb")))
int callee(int *x) { return *x; }

__attribute__((target("thumb")))
int test(int *x) { return callee(x); }

// arm: flags {"--load-base", "0x1000", "--address", "0x1001", "--raw-binary"}
// arm: go(prog)
// arm: ok()
