// Test that grease respects ELF symbol resolution order when multiple libraries
// export the same symbol. The executable links against libfoo.so then libbaz.so
// (both define foo()). Verifies that foo() from libfoo.so is used, matching the
// standard ELF behavior where the first library in link order takes precedence.

// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stub to foo() is skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// arm: ok()
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")
// TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): check("Calling `foo` from libfoo")
// TODO(#21): check_not "Skipped call to a PLT stub"

extern int *foo(void);
int test(void) {
    int *p = foo();
    return *p;
}
