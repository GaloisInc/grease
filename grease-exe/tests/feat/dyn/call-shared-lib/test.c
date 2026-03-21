// Test that grease can simulate calls to functions in shared libraries. The
// executable links against libfoo.so via DT_NEEDED and calls foo() through a
// PLT stub. Verifies automatic shared library discovery, explicit --shared-lib
// flags, --no-shared-libs behavior, and graceful fallback when libraries are
// missing.

// Run 1: --shared-lib-dir enables loading of DT_NEEDED shared libraries
// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x240:foo"}
// all: go(prog)
// Currently: PLT stubs are skipped (not simulated), but analysis succeeds.
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): x64: check("Calling 'foo' from shared library libfoo")
// TODO(#21): x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): arm: check("Calling 'foo' from shared library libfoo")
// TODO(#21): arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: check("Skipped call to a PLT stub")
// ppc32: check("foo")
// ppc32: ok()

// Run 2: --no-shared-libs disables shared library simulation
// TODO(#21): flags {"--no-shared-libs"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x240:foo"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// TODO(#21): x64: check("Shared library loading disabled")
// TODO(#21): x64: check_not "Loaded shared library"
// TODO(#21): x64: check_not "Calling 'foo'"
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// TODO(#21): arm: check("Shared library loading disabled")
// TODO(#21): arm: check_not "Loaded shared library"
// TODO(#21): arm: check_not "Calling 'foo'"
// arm: ok()
// ppc32: check("Skipped call to a PLT stub")
// ppc32: check("foo")
// ppc32: ok()

// Run 3: Missing SO in --shared-lib-dir (graceful degradation)
// TODO(#21): flags {"--shared-lib-dir", "/nonexistent"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x240:foo"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// TODO(#21): x64: check("Shared library not found: libfoo")
// TODO(#21): x64: check_not "Loaded shared library"
// TODO(#21): x64: check_not "Calling 'foo'"
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// TODO(#21): arm: check("Shared library not found: libfoo")
// TODO(#21): arm: check_not "Loaded shared library"
// TODO(#21): arm: check_not "Calling 'foo'"
// arm: ok()
// ppc32: check("Skipped call to a PLT stub")
// ppc32: check("foo")
// ppc32: ok()

// Run 4: Explicit --shared-lib works even with --no-shared-libs
// TODO(#21): flags {"--no-shared-libs"}
// TODO(#21): x64: flags {"--shared-lib", "tests/feat/dyn/libfoo.x64.so"}
// TODO(#21): arm: flags {"--shared-lib", "tests/feat/dyn/libfoo.armv7l.so"}
// TODO(#21): ppc32: flags {"--shared-lib", "tests/feat/dyn/libfoo.ppc32.so"}
// all: flags {"--symbol", "test"}
// ppc32: flags {"--plt-stub", "0x240:foo"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// TODO(#21): x64: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): x64: check("Calling 'foo' from shared library libfoo")
// TODO(#21): x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// TODO(#21): arm: check("Loaded shared library tests/feat/dyn/libfoo")
// TODO(#21): arm: check("Calling 'foo' from shared library libfoo")
// TODO(#21): arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: check("Skipped call to a PLT stub")
// ppc32: check("foo")
// ppc32: ok()

extern int *foo(void);
int test(void) {
    int *p = foo();
    return *p;
}
