/* Copyright (c) Galois, Inc. 2025 */

// CFLAGS: $LLVM -g

/// flags {"--symbol", "test"}
/// go(prog)

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct opaque;
struct rec { struct rec *p; };

int test(
    // * integers
    char x0,
    unsigned char x1,
    short x2,
    unsigned short x3,
    int x4,
    unsigned int x5,
    long x6,
    unsigned long x7,
    long long x8,
    unsigned long long x9,
    // ** typedefs
    bool x10,
    int8_t x11,
    uint8_t x12,
    int16_t x13,
    uint16_t x14,
    int32_t x15,
    uint32_t x16,
    int64_t x17,
    uint64_t x18,
    size_t x19,
    // * pointers
    void *x23,
    char *x24,
    struct opaque *x25,
    struct rec *x26,
    // ** qualifiers
    const char *x27,
    char* restrict x28,
    volatile char *x29
) {
  return x0;
}
/// precond=[[
/// %0: XX
/// %1: XX
/// %2: XX XX
/// %3: XX XX
/// %4: XX XX XX XX
/// %5: XX XX XX XX
/// %6: XX XX XX XX XX XX XX XX
/// %7: XX XX XX XX XX XX XX XX
/// %8: XX XX XX XX XX XX XX XX
/// %9: XX XX XX XX XX XX XX XX
/// %10: 
/// %11: XX
/// %12: XX
/// %13: XX XX
/// %14: XX XX
/// %15: XX XX XX XX
/// %16: XX XX XX XX
/// %17: XX XX XX XX XX XX XX XX
/// %18: XX XX XX XX XX XX XX XX
/// %19: XX XX XX XX XX XX XX XX
/// %20: 000000+0000000000000000
/// %21: 000001+0000000000000000
/// %22: 000002+0000000000000000
/// %23: 000003+0000000000000000
/// %24: 000004+0000000000000000
/// %25: 000005+0000000000000000
/// %26: 000006+0000000000000000
/// 
/// 000000: 
/// 000001: ##
/// 000002: 
/// 000003: ## ## ## ## ## ## ## ##
/// 000004: ##
/// 000005: 
/// 000006: 
/// ]]
/// -- TODO: Not clear to me why 05 and 06 are empty...
/// check "Using precondition:"
/// check(precond)
/// ok()
/// check "Final refined precondition:"
/// check(precond)
