// Test that grease can simulate calls to functions in shared libraries. The
// executable links against libfoo.so via DT_NEEDED and calls foo() through a
// PLT stub. Verifies automatic shared library discovery, explicit --shared-lib
// flags, --no-shared-libs behavior, and graceful fallback when libraries are
// missing.

// Run 1: --shared-lib-dir enables loading of DT_NEEDED shared libraries
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

// Run 2: --no-shared-libs disables shared library simulation
// x64: flags {"--no-shared-libs"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check("Shared library loading disabled")
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// x64: check_not "Loaded shared library"
// x64: ok()
// arm: flags {"--no-shared-libs"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: check("Shared library loading disabled")
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// arm: check_not "Loaded shared library"
// arm: ok()
// ppc32: flags {"--no-shared-libs"}
// ppc32: flags {"--symbol", "test"}
// ppc32: go(prog)
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")

// Run 3: Missing SO in --shared-lib-dir (graceful degradation)
// x64: flags {"--shared-lib-dir", "/nonexistent"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check("Shared library not found:")
// x64: check("Skipped call to a PLT stub")
// x64: check("foo")
// x64: check_not "Loaded shared library"
// x64: ok()
// arm: flags {"--shared-lib-dir", "/nonexistent"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: check("Shared library not found:")
// arm: check("Skipped call to a PLT stub")
// arm: check("foo")
// arm: check_not "Loaded shared library"
// arm: ok()
// ppc32: flags {"--shared-lib-dir", "/nonexistent"}
// ppc32: flags {"--symbol", "test"}
// ppc32: go(prog)
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")

// Run 4: Explicit --shared-lib works even with --no-shared-libs
// x64: flags {"--no-shared-libs"}
// x64: flags {"--shared-lib", "tests/feat/dyn/libfoo.x64.so"}
// x64: flags {"--symbol", "test"}
// x64: go(prog)
// x64: check("Loaded shared library")
// x64: check("from shared library libfoo")
// x64: check_not "Skipped call to a PLT stub"
// x64: ok()
// arm: flags {"--no-shared-libs"}
// arm: flags {"--shared-lib", "tests/feat/dyn/libfoo.armv7l.so"}
// arm: flags {"--symbol", "test"}
// arm: go(prog)
// arm: check("Loaded shared library")
// arm: check("from shared library libfoo")
// arm: check_not "Skipped call to a PLT stub"
// arm: ok()
// ppc32: flags {"--no-shared-libs"}
// ppc32: flags {"--shared-lib", "tests/feat/dyn/libfoo.ppc32.so"}
// ppc32: flags {"--symbol", "test"}
// ppc32: go(prog)
// ppc32: check("Pattern match failure")
// ppc32: check("Possible bug")

extern int *foo(void);
int test(void) {
    int *p = foo();
    return *p;
}
