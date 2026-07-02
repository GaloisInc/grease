/* Copyright (c) Galois, Inc. 2026 */

// Test GREASE's support for discovering and executing jump tables.

// x64: flags {"--symbol", "test"}
// x64: go(prog)

int test(unsigned char c)
{
    switch (c) {
    case 0x00 ... 0x1f:
    case 0x7f ... 0xff:
    case '\\': case '%': case '#': case '/':
    case '(': case ')': case '<': case '>':
    case '[': case ']': case '{': case '}':
        return 1;
    default:
        return 0;
    }
}
// x64: ok()
