// Test that grease respects ELF symbol resolution order when multiple libraries
// export the same symbol. The executable links against libfoo.so then libbaz.so
// (both define foo()). Verifies that foo() from libfoo.so is used, matching the
// standard ELF behavior where the first library in link order takes precedence.

// x64: flags {"--shared-lib-dir", "tests/feat/dyn"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check("Loaded shared library")
// x64: check("from shared library libfoo")
// x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: flags {"--shared-lib-dir", "tests/feat/dyn"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: check("Loaded shared library")
// arm: check("from shared library libfoo")
// arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: flags {"--shared-lib-dir", "tests/feat/dyn"}
// ppc32: flags {"--symbol", "test"}
// ppc32: go(prog)
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")

extern int *foo(void);
int test(void) {
    int *p = foo();
    return *p;
}
