/* Copyright (c) Galois, Inc. 2024 */

// CFLAGS: -emit-llvm -frecord-command-line

/// flags {"--symbol-startup-override", "test:tests/llvm-bc/extra/startup-override.llvm.cbl"}
/// go(prog)

#include <stddef.h>
#include <stdint.h>
#include <string.h>

void test(uint8_t *buf, size_t sz) {
  memset(buf, 'a', sz);
}
/// ok()
