/* Copyright (c) Galois, Inc. 2024 */

// flag: --req no-mprotect
// flag(ppc32): --plt-stub 0x10000230:mprotect

#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>

int test() {
  uint8_t func[8] = {0};
  mprotect((void *)func, sizeof(func), PROT_READ | PROT_EXEC);
  return 1;
}
// all: req_failed()
// all: check "Cannot call mprotect"
