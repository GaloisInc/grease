/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <assert.h>
#include <stdbool.h>
void test() { assert(false); }
// TODO: Ideally, these would all report a call to assert.
// all: must_fail()
// arm: check "Call to assert()"
// ppc32: check "Could not identify block"
// x64: check "Could not identify block"
