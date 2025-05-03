// A regression test for gitlab#249. What makes this test unique is that this
// is compiled to a shared library rather than a standalone executable, and as
// a result, most compilers will compile the call to `deref` in `test` to a PLT
// call rather than a direction function call. As such, this test case ensures
// that grease can resolve PLT calls to functions that occur within the same
// shared library.

// all: flags {"--symbol", "test"}
// all: go(prog)

int deref(int *x) { return *x; }
// arm: check "Calling a PLT stub (deref)"
// x64: check "Calling a PLT stub (deref)"
int test(int *x) { return deref(x); }
// TODO: Why does PPC fail here?
// arm: ok()
// ppc32: check "Could not identify block at 0x10000220"
// x64: ok()
