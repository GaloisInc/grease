/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <stdlib.h>
void test(void) { exit(EXIT_FAILURE); }
// all: must_fail()
