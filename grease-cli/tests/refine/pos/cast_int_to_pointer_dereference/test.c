/* Copyright (c) Galois, Inc. 2024 */

// Should behave the same as just having a pointer parameter
#include <stdint.h>
int test(uintptr_t ptr) { return *((int *)(void *)ptr); }
