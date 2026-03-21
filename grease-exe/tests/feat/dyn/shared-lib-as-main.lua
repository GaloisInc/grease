-- Test simulating a shared library directly as the main target (not via an
-- executable). Verifies that grease can analyze shared libraries with
-- --shared-lib-dir to resolve their own dependencies.

---- x64 ----

-- Run 1: libfoo.so as main target (no dependencies)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.x64.so"
ok()

-- Run 2: libbar.so as main target (depends on libfoo.so transitively)
-- TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
-- Currently: x64 follows PLT stubs within the same .so (not skipped).
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
-- TODO(#21): check("Calling `foo` from libfoo")
-- TODO(#21): check_not "Skipped call to a PLT stub"

-- Run 3: libbar.so with --no-shared-libs (disables dependency loading)
-- TODO(#21): flags {"--no-shared-libs"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Shared library loading disabled")
-- TODO(#21): check_not "Loaded shared library"
-- TODO(#21): check_not "Calling `foo`"

-- Run 4: libbar.so with missing SO (graceful degradation)
-- TODO(#21): flags {"--shared-lib-dir", "/nonexistent"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.x64.so"
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Shared library not found: libfoo")
-- TODO(#21): check_not "Calling `foo`"

---- armv7l ----

-- Run 5: libfoo.so as main target (no dependencies, ARM)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.armv7l.so"
ok()

-- Run 6: libbar.so as main target (depends on libfoo.so transitively, ARM)
-- TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
-- Currently: bar calls foo through PLT stub, but no shared lib support yet.
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
-- TODO(#21): check("Calling `foo` from libfoo")
-- TODO(#21): check_not "Skipped call to a PLT stub"

-- Run 7: libbar.so with --no-shared-libs (ARM)
-- TODO(#21): flags {"--no-shared-libs"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Shared library loading disabled")
-- TODO(#21): check_not "Loaded shared library"
-- TODO(#21): check_not "Calling `foo`"

-- Run 8: libbar.so with missing SO (ARM)
-- TODO(#21): flags {"--shared-lib-dir", "/nonexistent"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.armv7l.so"
check("Skipped call to a PLT stub")
check("foo")
ok()
-- TODO(#21): check("Shared library not found: libfoo")
-- TODO(#21): check_not "Calling `foo`"

---- ppc32 ----

-- Run 9: libfoo.so as main target (no dependencies, PPC32)
flags {"--symbol", "foo"}
go "tests/feat/dyn/libfoo.ppc32.so"
-- Currently: PPC32 can't identify the block (known limitation).
check("Possible bug")
-- TODO(#21): ok()

-- Run 10: libbar.so as main target (PPC32)
-- TODO(#21): flags {"--shared-lib-dir", "tests/feat/dyn"}
flags {"--symbol", "bar"}
go "tests/feat/dyn/libbar.ppc32.so"
-- Currently: PPC32 can't identify the block (known limitation).
check("Possible bug")
-- TODO(#21): check("Loaded shared library tests/feat/dyn/libfoo")
-- TODO(#21): check("Calling `foo` from libfoo")
