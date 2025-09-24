/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: flags {"--req", "in-text"}
// all: go(prog)

// The correct precondition for this function is complex... Not clear if it's
// worth it to bake this logic into grease.
int test(int (*fun_ptr)()) { return fun_ptr(); }
// TODO: Code discovery failure (bug in Macaw):
// arm: check "TopV where PSTATE_T expected"
// ppc32: req_failed()
// ppc32: check ".text"
// x64: req_failed()
// x64: check ".text"
