/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#219 which ensures that we do not attempt to use
// a buggy override for `printf`. The current approach is that `grease` will
// not register a built-in override for `printf` at all, instead skipping any
// calls to `printf` during simulation.

// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x10000220:printf"}
// all: go(prog)

#include <stdio.h>

void test(void) {
  printf("Blah: %d\n", 27);
}
// all: ok()
