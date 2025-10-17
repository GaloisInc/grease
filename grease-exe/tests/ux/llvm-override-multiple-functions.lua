-- Test the UX of our error messages when multiple functions with same name
-- are found

path = "tests/ux/extra/llvm-override-multiple-functions.llvm.cbl"
flags {"--symbol", "test", "--overrides", path}
go "tests/llvm/load.llvm.cbl"
user_error "Override contains multiple 'llvm-override-multiple-functions' functions"
