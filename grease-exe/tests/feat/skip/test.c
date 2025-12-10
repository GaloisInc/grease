/* Copyright (c) Galois, Inc. 2025 */

// Test --skip for binaries.

void call(void (*fun_ptr)()) {
  fun_ptr();
}

void test(void (*fun_ptr)()) {
  call(fun_ptr);
}

// all: flags {"--error-symbolic-fun-calls"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// all: must_fail()

// all: flags {"--error-symbolic-fun-calls"}
// all: flags {"--skip", "call"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// all: ok()
