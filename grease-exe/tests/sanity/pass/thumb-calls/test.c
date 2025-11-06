/* Copyright (c) Galois, Inc. 2025 */

// Simple call test to make sure calling thumb mode functions works.

__attribute__((target("thumb"))) int callee(int *x) { return *x; }

__attribute__((target("thumb"))) int test(int *x) { return callee(x); }

// arm: flags {"--address", "0x1008b"}
// arm: go(prog)

// TODO: Ideally, Macaw's code discovery would be smart enough to figure this
// out, but it currently isn't.
// arm: check "Could not identify block"
