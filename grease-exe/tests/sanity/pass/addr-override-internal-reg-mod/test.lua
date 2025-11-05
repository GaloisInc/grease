flags {"--symbol", "test"}
-- Address of the nop after setting rsi == rdi function from `objdump -d`.
flags {"--addr-override", "0x10001003:tests/sanity/pass/addr-override-internal-reg-mod/extra/addr-override.x86_64.cbl"}
-- We check that we can observe the change in rsi state from the middle of the block
-- from an address override. Regression test for
-- https://github.com/GaloisInc/grease/issues/416
go "tests/sanity/pass/addr-override-internal-reg-mod/extra/test.x64.elf"
ok()