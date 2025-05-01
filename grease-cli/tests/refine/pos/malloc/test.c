/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test
// flags(ppc32): --plt-stub 0x10000220:malloc

#include <stdlib.h>
void *test(void) { return malloc(1); }
// all: ok()
