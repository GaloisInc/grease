/* Copyright (c) Galois, Inc. 2024 */

// CFLAGS: $STATIC_WITH_LIBS

// all: flags {"--symbol", "test"}
// all: go(prog)

#include <sys/syscall.h>
#include <unistd.h>

void test(void) {
  getppid();
}
// all: ok()
