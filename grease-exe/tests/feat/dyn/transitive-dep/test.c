// Test that grease can handle transitive shared library dependencies. The
// executable links against libbar.so (which itself depends on libfoo.so) and
// calls bar() which internally calls foo(). Verifies that grease correctly
// loads transitive DT_NEEDED dependencies.

// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stub to bar() is skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("bar")
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("bar")
// arm: ok()
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")
// TODO(#21): check("Loaded shared library tests/feat/dyn/libbar")
// TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): check("Calling `bar` from libbar")
// TODO(#21): check_not "Skipped call to a PLT stub"

extern int *bar(void);
int test(void) {
    int *p = bar();
    return *p;
}
