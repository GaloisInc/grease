-- Test screach against an ECFS snapshot.
--
-- The fixture is the checked-in snapshot from the ghidra-ecfs extension; it
-- captures a process that calls add_numbers() in a shared library.

local ecfs = "../ghidra-ecfs/src/test/resources/ecfs/test.x64.elf"

-- Reach a shared-library function via --target-symbol.
flags {"--entry-symbol", "main"}
flags {"--target-symbol", "add_numbers"}
go(ecfs)
-- add_numbers is in binSymMap (from ecfsDynsym), so the resolved address
-- appears in brackets on the "Searching" line.
check "Searching for target function 'add_numbers' [address "
reached "add_numbers"
verified()

-- Reach the same function via its raw virtual address.
flags {"--entry-symbol", "main"}
flags {"--target-addr", "0x707981bcc0f9"}
go(ecfs)
check "Reached target address 0x707981bcc0f9 [in function 'add_numbers'"
verified()
