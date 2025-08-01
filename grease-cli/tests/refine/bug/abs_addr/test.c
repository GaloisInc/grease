/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

int __attribute__((noinline)) deref(int *p) { return *p; }

int test(void) { return deref((int *)(void *)0xdeadbeef); }
// all: check "0xdeadbeef"
