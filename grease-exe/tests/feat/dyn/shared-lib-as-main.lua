-- Test simulating a shared library directly as the main target (not via an
-- executable). Verifies that grease can analyze shared libraries with
-- --shared-lib-dir to resolve their own dependencies.

---- x64 ----

-- Run 1: libfoo.so as main target (no dependencies)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.x64.so"
ok()

-- Run 2: libbar.so as main target (depends on libfoo.so transitively)
flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
check("Loaded shared library")
check("from shared library libfoo")
check_not "Skipped call to a PLT stub"
ok()

-- Run 3: libbar.so with --no-shared-libs (disables dependency loading)
flags {"--no-shared-libs"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
check("Shared library loading disabled")
check("Skipped call to a PLT stub")
check("foo")
check_not "Loaded shared library"
ok()

-- Run 4: libbar.so with missing SO (graceful degradation)
flags {"--shared-lib-dir", "/nonexistent"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
check("Shared library not found:")
check("Skipped call to a PLT stub")
check("foo")
check_not "Loaded shared library"
ok()

---- armv7l ----

-- Run 5: libfoo.so as main target (no dependencies, ARM)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.armv7l.so"
ok()

-- Run 6: libbar.so as main target (depends on libfoo.so transitively, ARM)
flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
check("Loaded shared library")
check("from shared library libfoo")
check_not "Skipped call to a PLT stub"
ok()

-- Run 7: libbar.so with --no-shared-libs (ARM)
flags {"--no-shared-libs"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
check("Shared library loading disabled")
check("Skipped call to a PLT stub")
check("foo")
check_not "Loaded shared library"
ok()

-- Run 8: libbar.so with missing SO (ARM)
flags {"--shared-lib-dir", "/nonexistent"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
check("Shared library not found:")
check("Skipped call to a PLT stub")
check("foo")
check_not "Loaded shared library"
ok()

---- ppc32 ----

-- Run 9: libfoo.so as main target (no dependencies, PPC32)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.ppc32.so"
-- PPC32 can't identify the block (known limitation).
check("Possible bug")

-- Run 10: libbar.so as main target (PPC32)
flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.ppc32.so"
-- PPC32 can't identify the block (known limitation).
check("Possible bug")
