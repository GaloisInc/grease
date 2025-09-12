-- Test the UX of our error messages when expected function is not found in
-- LLVM override

path = "tests/ux/extra/llvm-override-function-not-found.llvm.cbl"
flags {"--symbol", "test", "--overrides", path}
go "tests/llvm/load.llvm.cbl"
exception()
check "user error: Expected to find a function named 'llvm-override-function-not-found'"
