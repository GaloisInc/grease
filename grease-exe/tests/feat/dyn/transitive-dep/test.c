// Test that grease can handle transitive shared library dependencies. The
// executable links against libbar.so (which itself depends on libfoo.so) and
// calls bar() which internally calls foo(). Verifies that grease correctly
// loads transitive DT_NEEDED dependencies.

// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x240:bar"}
// all: go(prog)
// Currently: PLT stub to bar() is skipped (no shared lib support yet).
// all: check("Skipped call to a PLT stub")
// x64: check("bar")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libbar")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): x64: check("Calling 'bar' from shared library libbar")
// TODO(#21): x64: check("Calling 'foo' from shared library libfoo")
// TODO(#21): x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: check("bar")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libbar")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): arm: check("Calling 'bar' from shared library libbar")
// TODO(#21): arm: check("Calling 'foo' from shared library libfoo")
// TODO(#21): arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: check("bar")
// ppc32: ok()

extern int *bar(void);
int test(void) {
    int *p = bar();
    return *p;
}
