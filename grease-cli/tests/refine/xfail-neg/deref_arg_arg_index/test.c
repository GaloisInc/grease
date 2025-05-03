/* Copyright (c) Galois, Inc. 2024 */

// grease does not yet support this kind of precondition that depends on a
// relation between two arguments

// all: flags {"--symbol", "test"}

// Set --iters low to keep this test fast
// all: flags {"--iters", "8"}
// all: go(prog)

int test(int *ptr, unsigned int idx) { return ptr[idx]; }
// arm: could_not_infer()
// TODO(#47):
// ppc32: check "maximum iterations exceeded"
// x64: check "maximum iterations exceeded"
