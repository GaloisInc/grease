// Test that grease can handle transitive shared library dependencies. The
// executable links against libbar.so (which itself depends on libfoo.so) and
// calls bar() which internally calls foo(). Verifies that grease correctly
// loads transitive DT_NEEDED dependencies.

// x64: flags {"--shared-lib-dir", "tests/feat/dyn"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check("Loaded shared library")
// x64: check("from shared library libbar")
// x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: flags {"--shared-lib-dir", "tests/feat/dyn"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: check("Loaded shared library")
// arm: check("from shared library libbar")
// arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: flags {"--shared-lib-dir", "tests/feat/dyn"}
// ppc32: flags {"--symbol", "test"}
// ppc32: go(prog)
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")

extern int *bar(void);
int test(void) {
    int *p = bar();
    return *p;
}
