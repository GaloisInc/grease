flags {"--symbol", "test"}
-- Address of the `test` function from `objdump -d`.
flags {"--addr-override", "0x10001003:tests/sanity/pass/addr-override-internal-reg-mod/extra/addr-override.x86_64.cbl"}
go "tests/sanity/pass/addr-override-internal-reg-mod/extra/test.x64.elf"
ok()