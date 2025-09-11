-- Test the UX of our error messages when expected function is not found in address override

path = "tests/ux/extra/addr-override-function-not-found.x64.cbl"
flags {"--addr-override", "0x401000:" .. path}
go "tests/refine/pos/compare_to_null/test.x64.elf"
exception()
check "user error: Expected to find a function named 'addr-override-function-not-found'"
