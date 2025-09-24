-- Copyright (c) Galois, Inc. 2025

-- Ensure that address overrides work as expected.

flags {"--symbol", "test"}
-- Address of the `test` function from `objdump -d`.
flags {"--addr-override", "0x401000:tests/refine/bug/extra/addr-override.x64.cbl"}
-- This can be any test that is expected to refine properly.
go "tests/refine/pos/compare_to_null/test.x64.elf"
must_fail()
