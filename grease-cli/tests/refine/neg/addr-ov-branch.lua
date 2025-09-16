-- Copyright (c) Galois, Inc. 2025

-- Regression test for #376. Ensures that address overrides can branch.

flags {"--symbol", "test"}
-- Address of the `test` function from `objdump -d`.
flags {"--addr-override", "0x401000:tests/refine/neg/extra/addr-override-branch.x64.cbl"}
-- This can be any test that is expected to refine properly.
go "tests/refine/pos/compare_to_null/test.x64.elf"
exception()
-- TODO(#376): Don't throw an exception here
check "Address override did not return a result nor abort"
