// Test that grease can simulate calls to functions in shared libraries. The
// executable links against libfoo.so via DT_NEEDED and calls foo() through a
// PLT stub. Verifies automatic shared library discovery, explicit --shared-lib
// flags, --no-shared-libs behavior, and graceful fallback when libraries are
// missing.

// Run 1: Default (with shared lib simulation via --shared-lib-dir)
// TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stubs are skipped (not simulated), but analysis succeeds.
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

// Run 2: --no-shared-libs disables shared library simulation
// TODO(#21): flags {"--no-shared-libs"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// arm: ok()
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")
// TODO(#21): check("Shared library loading disabled")
// TODO(#21): check_not "Loaded shared library"
// TODO(#21): check_not "Calling `foo`"

// Run 3: Missing SO in --shared-lib-dir (graceful degradation)
// TODO(#21): flags {"--shared-lib-dir", "/nonexistent"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// x64: ok()
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// arm: ok()
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")
// TODO(#21): check("Shared library not found: libfoo")
// TODO(#21): check_not "Calling `foo`"

// Run 4: Explicit --shared-lib works even with --no-shared-libs
// TODO(#21): flags {"--no-shared-libs"}
// TODO(#21): flags {"--shared-lib", "tests/feat/dyn/libfoo.x64.so"}
// all: flags {"--symbol", "test"}
// all: go(prog)
// Currently: PLT stubs are skipped (no shared lib support yet).
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
