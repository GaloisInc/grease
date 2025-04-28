/* Copyright (c) Galois, Inc. 2024 */

// The code in this test case is not particularly interesting. The interesting
// part is that we strip the binary after compiling it, which requires that
// grease supply an address in order to discover the `test` function. This also
// serves as a regression test for gitlab#110.

// flags(arm): --address 0x10074
// flags(ppc32): --address 0x10000074
// flags(x64): --address 0x401000

int test(int *x) {
  return *x == 42;
}
// all: ok()
