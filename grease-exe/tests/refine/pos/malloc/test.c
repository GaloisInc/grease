/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x10000220:malloc"}
// all: go(prog)

#include <stdlib.h>
void *test(void) { return malloc(1); }
// all: ok()
