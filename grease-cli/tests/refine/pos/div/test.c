/* Copyright (c) Galois, Inc. 2024 */

// A regression test for gitlab#215#note_215215,
// which critically depends on macaw-symbolic's semantics for division-related
// instructions being correct. Although there is potential for a double-free to
// occur when invoking `foo`, this should not happen when analyzing `test` as an
// entrypoint, as the concrete arguments it passes to `foo` should avoid any
// code paths that would use `free` on `p` twice.
#include <stddef.h>

// Redefine `malloc` and `free`. We won't actually use these implementations
// during simulation, as grease will override them. We do this to make it easier
// to produce an x86-64 binary, as grease's test case currently has issues
// creating minimal x86-64 binaries that dynamically link against libc.
void *malloc(size_t size) {
  return 0;
}
void free(void *ptr) {}

void foo(void *p, int x) {
  if (x > 0 && x < 10 && x % 2 == 0) {
    free(p);
  }
  if (x % 3 == 0) {
    free(p);
  }
}

void test(void) {
  void *p = malloc(8);
  foo(p, 0x01);
}
