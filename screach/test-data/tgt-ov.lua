-- Test the functionality of target overrides and the `@reached` built-in.
--
-- The underlying test-case (i.e., ELF file) isn't super important.

flags {"--globals", "symbolic"}
flags {"--entry-symbol", "process_command"}
flags {"--target-addr", "0x0370"}
flags {"--target-override", "test-data/extra/tgt-ov-feasible.cbl"}
go "test-data/global.elf"
reached "reached"
check "rcx: 00000000deadbeef"

flags {"--globals", "symbolic"}
flags {"--entry-symbol", "process_command"}
flags {"--target-addr", "0x0370"}
flags {"--target-override", "test-data/extra/tgt-ov-infeasible.cbl"}
go "test-data/global.elf"
failed_to_reach()
