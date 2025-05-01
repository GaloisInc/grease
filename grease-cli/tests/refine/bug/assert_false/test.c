/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test

#include <assert.h>
#include <stdbool.h>
void test() { assert(false); }
// TODO: Ideally, these would all report a call to assert.
// arm: must_fail()
// arm: check "Call to assert()"
// ppc32: could_not_infer()
// ppc32: check "Could not identify block"
// x64: must_fail()
