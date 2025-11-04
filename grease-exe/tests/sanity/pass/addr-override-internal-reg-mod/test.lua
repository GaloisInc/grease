flags {"--symbol", "test"}
-- Address of the `test` function from `objdump -d`.
flags {"--addr-override", "0x1003:tests/sanity/pass/addr-override-internal-reg-mod/add-override.x64_64.cbl"}
go "tests/sanity/pass/addr-override-internal-reg-mod/test.x64.elf"
ok()