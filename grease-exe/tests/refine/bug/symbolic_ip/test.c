/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#182. The address for &f in the right-hand side
// of g's definition will be stored in the global address space, which will
// appear (at first glance) as a symbolic address. We must consult an SMT
// solver to conclude that the address is in fact concrete.

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stdbool.h>

void f(void) {
    int* x;
    int y = *x; // uninitialized stack read
}
void (*g)(void) = &f;

void test(void) {
  g();
}
// all: uninit_stack()
