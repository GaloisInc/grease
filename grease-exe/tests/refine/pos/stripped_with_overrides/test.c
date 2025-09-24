/* Copyright (c) Galois, Inc. 2025 */

// We strip this binary after compiling it, /and/ we need to override my_malloc
// and my_free. To teach grease the entrypoint to the binary, we use the
// --address flag. To teach grease which functions to override, we use the
// --overrides-yaml flag.

// arm: flags {"--address", "0x100b8"}
// arm: flags {"--overrides-yaml", "tests/refine/pos/stripped_with_overrides/test-overrides.aux.armv7l.yaml"}
// ppc32: flags {"--address", "0x100000c0"}
// ppc32: flags {"--overrides-yaml", "tests/refine/pos/stripped_with_overrides/test-overrides.aux.ppc32.yaml"}
// x64: flags {"--address", "0x401020"}
// x64: flags {"--overrides-yaml", "tests/refine/pos/stripped_with_overrides/test-overrides.aux.x86_64.yaml"}
// all: go(prog)

#include <stddef.h>

void* my_malloc(size_t size) {
  return NULL;
}

void my_free(void* ptr) {}

void test(void) {
  int* x = my_malloc(sizeof(int));
  *x = 42;
  my_free(x);
}
// all: ok()
