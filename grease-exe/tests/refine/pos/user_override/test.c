/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// all: flags {"--overrides", "tests/refine/pos/user_override/shouldnt_loop.aux.cbl"}
// all: go(prog)

void shouldnt_loop(void) {
  while (1) {}
}

void test(void) {
  shouldnt_loop();
}
// all: ok()
