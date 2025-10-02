-- Test the UX of our error messages for bad argument types in address overrides

path = "tests/ux/extra/addr-override-bad-args.x64.cbl"
flags {"--addr-override", "0x401000:" .. path}
go "tests/refine/pos/compare_to_null/test.x64.elf" 
user_error("Bad address override argument types at " .. path)
