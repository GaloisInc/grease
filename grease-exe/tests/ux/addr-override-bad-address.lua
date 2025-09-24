-- Test the UX of our error messages for a bad address for an address override

path = "tests/refine/pos/extra/addr-override.x64.cbl"
flags {"--addr-override", "0x99999999:" .. path}
go "tests/refine/pos/compare_to_null/test.x64.elf" 
exception()
check("user error: Bad address: 0x99999999 at " .. path)
