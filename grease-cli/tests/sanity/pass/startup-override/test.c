/* Copyright (c) Galois, Inc. 2024 */

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
