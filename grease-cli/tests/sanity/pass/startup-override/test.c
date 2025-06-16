/* Copyright (c) Galois, Inc. 2024 */

// all: flags {"--symbol", "test"}
// arm: flags {"--symbol-startup-override", "test:tests/sanity/pass/startup-override/startup-override.aux.armv7l.cbl"}
// ppc32: flags {"--symbol-startup-override", "test:tests/sanity/pass/startup-override/startup-override.aux.ppc32.cbl"}
// x64: flags {"--symbol-startup-override", "test:tests/sanity/pass/startup-override/startup-override.aux.x86_64.cbl"}
// all: go(prog)

#include <stddef.h>
#include <stdint.h>
#include <string.h>

void test(uint8_t *buf, size_t sz) {
  memset(buf, 'a', sz);
}
// arm: ok()
// ppc32: ok()
// x64: ok()
