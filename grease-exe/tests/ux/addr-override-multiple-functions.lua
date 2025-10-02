-- Test the UX of our error messages when multiple functions with same name are found

path = "tests/ux/extra/addr-override-multiple-functions.x64.cbl"
flags {"--addr-override", "0x401000:" .. path}
go "tests/refine/pos/compare_to_null/test.x64.elf" 
user_error "Override contains multiple 'addr-override-multiple-functions' functions"
