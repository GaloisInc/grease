// Test that grease respects ELF symbol resolution order when multiple libraries
// export the same symbol. The executable links against libfoo.so then libbaz.so
// (both define foo()). Verifies that foo() from libfoo.so is used, matching the
// standard ELF behavior where the first library in link order takes precedence.

// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x250:foo"}
// all: go(prog)
// Currently: PLT stub to foo() is skipped (no shared lib support yet).
// all: check("Skipped call to a PLT stub")
// x64: check("foo")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libbaz")
// TODO(#21): x64: check("Calling 'foo' from shared library libfoo")
// TODO(#21): x64: check_not "Calling 'foo' from shared library libbaz"
// TODO(#21): x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: check("foo")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libbaz")
// TODO(#21): arm: check("Calling 'foo' from shared library libfoo")
// TODO(#21): arm: check_not "Calling 'foo' from shared library libbaz"
// TODO(#21): arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: check("foo")
// ppc32: ok()

extern int *foo(void);
int test(void) {
    int *p = foo();
    return *p;
}
