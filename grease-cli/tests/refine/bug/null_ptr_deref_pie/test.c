// A regression test for gitlab#254 and gitlab#255. This ensures that
// dereferencing a null pointer will be flagged as a possible bug even when
// analyzing a position-independent executable (PIE). In PIE binaries, it is
// common for the the memory at address 0x0 to be mapped to something valid,
// but the null pointer is defined to be the address 0x0 as well, so `grease`
// must load the memory at address 0x0 to virtual addresses with a constant
// offset to ensure that the two do not overlap in the virtual address space.

__attribute__((noinline)) int f(int *in) { return *in; }

int test(void) {
  return f(0);
}
