/* Copyright (c) Galois, Inc. 2024 */

// flags: --overrides tests/refine/pos/user_override/shouldnt_loop.cbl

void shouldnt_loop(void) {
  while (1) {}
}

void test(void) {
  shouldnt_loop();
}
// all: ok()
