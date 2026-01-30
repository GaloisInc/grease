-- Copyright (c) Galois, Inc. 2025

-- Test the UX of error messages when an override doesn't have a function with
-- the right name.
--
-- Regression test for #478.

flags {"--overrides", "tests/ux/extra/override-bad-name.x64.cbl"}
flags {"--symbol", "test"}
-- This can be any test that is expected to refine properly.
go "tests/refine/pos/compare_to_null/test.x64.elf"
user_error "Expected to find a function named 'override-bad-name'"
