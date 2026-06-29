-- Test screach against an ECFS snapshot.
--
-- The fixture is the checked-in snapshot from the ghidra-ecfs extension; it
-- captures a process that calls add_numbers() in a shared library.

local ecfs = "../ghidra-ecfs/src/test/resources/ecfs/test.x64.elf"

-- Reach a shared-library function via --target-symbol.
--
-- TODO: binSymMap is not populated with ECFS dynamic symbols yet, so the
-- "Searching" log line does not include the resolved address in brackets.
-- Once that is fixed this check should be updated to:
--   checkln "Searching for target function 'add_numbers' [address "
flags {"--entry-symbol", "main"}
flags {"--target-symbol", "add_numbers"}
go(ecfs)
checkln "Searching for target function 'add_numbers'"
reached "add_numbers"
verified()

-- Reach the same function via its raw virtual address.
--
-- TODO: the address-based goal evaluator never fires because
-- greaseMacawExtImpl's extensionExec short-circuits for MacawInstructionStart
-- without calling through to the wrapped extension.  Once that is fixed this
-- check should be updated to assert REACHED instead.
flags {"--entry-symbol", "main"}
flags {"--target-addr", "0x707981bcc0f9"}
go(ecfs)
check "Failed to reach target!"
