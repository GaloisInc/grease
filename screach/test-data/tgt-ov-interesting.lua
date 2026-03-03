-- Test the functionality of target overrides and the `@reached` built-in on an
-- interesting example.

flags {"--entry-symbol", "go"}
flags {"--explore"}
flags {"--callgraph", "./test-data/fig2-sdse-cg.csv"}
flags {"--target-addr", "0x25b3"}
flags {"--target-containing-function-address", "0x2550"}
flags {"--target-override", "test-data/extra/tgt-ov-interesting.cbl"}
go "test-data/fig2-sdse.elf"
reached "reached"
check [[
000001: 62
000002: 62
000003: 62
000004: 62
]]
-- RCX add happens before the override so we should get limited to 4 based on the override
check_not "000005"
