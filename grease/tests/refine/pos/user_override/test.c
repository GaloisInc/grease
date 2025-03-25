/* Copyright (c) Galois, Inc. 2024 */

void shouldnt_loop(void) {
  while (1) {}
}

void test(void) {
  shouldnt_loop();
}
