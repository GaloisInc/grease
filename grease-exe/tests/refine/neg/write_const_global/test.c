/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

const int x = 4;
__attribute__((noinline)) void make_it_five(int *ptr) { *ptr = 5; }
int test(int y) {
  make_it_five(&x);
  return y + x;
}
// arm: must_fail()
// x64: must_fail()

// TODO: https://github.com/GaloisInc/macaw/issues/418
// ppc32: ok()
