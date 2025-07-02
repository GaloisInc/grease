/* Copyright (c) Galois, Inc. 2025 */

// CFLAGS: $LLVM -g

/// flags {"--symbol", "test"}
/// flags {"--use-debug-info-types"}
/// go(prog)

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

struct opaque;
struct rec { struct rec *p; };
struct list {
    int data; // 4 bytes
    // padding: 4 bytes
    struct list *tail;  // 8 bytes
    // total: 4 + 4 + 8 = 16 bytes
};
struct pad {
    uint8_t a; // 1 byte
    // padding: 7 bytes
    uint64_t b; // 8 bytes
    // total: 1 + 7 + 8 = 16 bytes
};
union u {
    uint8_t a;
    void *p;
};

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
    void *x20,
    char *x21,
    struct opaque *x22,
    struct rec *x23,
    struct list *x24,
    struct pad *x25,
    // ** qualifiers
    const char *x26,
    char* restrict x27,
    volatile char *x28,
    // * unions
    union u x29,
    // * floating-point
    float x30,
    double x31,
    long double x32
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
/// %24: 000005+0000000000000000
/// %25: 000007+0000000000000000
/// %26: 000008+0000000000000000
/// %27: 000009+0000000000000000
/// %28: 00000a+0000000000000000
/// %29: 00000b+0000000000000000
/// %30: Float
/// %31: Double
/// %32: X86_80
/// 
/// 000000: 
/// 000001: ##
/// 000002: 
/// 000003: 000004+0000000000000000
/// 000004: 
/// 000005: ## ## ## ## ## ## ## ## 000006+0000000000000000
/// 000006: 
/// 000007: ##*10
/// 000008: ##
/// 000009: 
/// 00000a: 
/// 00000b: 
/// ]]
/// -- NOTE: 09 and 0a are empty because `Info` doesn't handle qualifiers:
/// -- https://github.com/GaloisInc/llvm-pretty/issues/162
/// check "Using precondition:"
/// check(precond)
/// ok()
/// check "Final refined precondition:"
/// check(precond)
