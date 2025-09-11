-- Test the UX of our error messages for bad return type in address overrides

path = "tests/ux/extra/addr-override-bad-return.x64.cbl"
flags {"--addr-override", "0x401000:" .. path}
go "tests/refine/pos/compare_to_null/test.x64.elf" 
exception()
check("user error: Bad address override return type at " .. path)
