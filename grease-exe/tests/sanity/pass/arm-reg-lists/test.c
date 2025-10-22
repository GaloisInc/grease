/* Copyright (c) Galois, Inc. 2025 */

// Simple arm test to make sure pop register lists works

int callee(int *x) { return *x; }

int test(int *x) { return callee(x); }

// TODO(#432): The pop in callee should succeed but gives uninit stack

// arm: flags {"--address", "0x1008b"}
// arm: go(prog)
// arm: uninit_stack()
