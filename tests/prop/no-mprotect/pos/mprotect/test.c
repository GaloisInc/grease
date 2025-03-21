/* Copyright (c) Galois, Inc. 2024 */

#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>

int test() {
  uint8_t func[8] = {0};
  mprotect((void *)func, sizeof(func), PROT_READ | PROT_EXEC);
  return 1;
}
