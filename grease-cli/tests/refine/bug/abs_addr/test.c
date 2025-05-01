/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

int __attribute__((noinline)) deref(int *p) { return *p; }

int test(void) { return deref((int *)(void *)0xdeadbeef); }
// all: must_fail()
// all: check "0xdeadbeef"
