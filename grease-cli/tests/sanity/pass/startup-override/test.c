/* Copyright (c) Galois, Inc. 2024 */

// flags: --symbol test
// flags(arm): --symbol-startup-override test:tests/sanity/pass/startup-override/startup-override.aux.armv7l.cbl
// flags(ppc32): --symbol-startup-override test:tests/sanity/pass/startup-override/startup-override.aux.ppc32.cbl
// flags(x64): --symbol-startup-override test:tests/sanity/pass/startup-override/startup-override.aux.x86_64.cbl

#include <stddef.h>
#include <stdint.h>
#include <string.h>

void test(uint8_t *buf, size_t sz) {
  memset(buf, 'a', sz);
}
// arm: ok()
// TODO: Why does PPC fail here?
// ppc32: check "PPCInvalidInstruction"
// x64: ok()
