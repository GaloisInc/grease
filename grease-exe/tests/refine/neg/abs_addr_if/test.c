/* Copyright (c) Galois, Inc. 2024 */

// This is a regression test for gitlab#239

// all: flags {"--symbol", "test"}
// all: go(prog)

int __attribute__((noinline)) deref(int *p) { return *p; }

int test(int x) {
  if (x)
    return deref((int *)(void *)0xdeadbeef);
  return 0;
}
// all: could_not_infer()
