/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int __attribute__((noinline)) deref(int *p) { return *p; }

int test(void) { return deref((int *)(void *)0xdeadbeef); }
// all: check "0xdeadbeef"
// TODO(#307) this test should mustfail because there is no way to refine this address 
// We currently exclude considering unmapped memory address errors from consideration in mustfail so 
// that we can refine SymBV initial states to heap pointers.
