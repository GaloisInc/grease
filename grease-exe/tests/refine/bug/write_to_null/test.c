/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: go(prog)

__attribute__((noinline)) void make_it_five(int *ptr) { *ptr = 5; }
void test() { make_it_five(0); }
// all: must_fail()
