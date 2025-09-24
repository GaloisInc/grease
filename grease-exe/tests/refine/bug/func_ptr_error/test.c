/* Copyright (c) Galois, Inc. 2024 */

// A variant of the tests/prop/in-text/pos/func_ptr/test.c test case where
// grease is invoked with --error-symbolic-fun-calls, which causes grease to
// interpret the call to the symbolif function pointer as a bug.

// all: flags {"--symbol", "test"}
// all: flags {"--error-symbolic-fun-calls"}
// all: go(prog)

int test(int (*fun_ptr)()) { return fun_ptr(); }
// all: must_fail()
// all: check "Cannot resolve a symbolic function address"
