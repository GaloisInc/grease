-- Test the UX of our error messages for unsupported types in LLVM overrides

path = "tests/ux/extra/llvm-override-unsupported-type.llvm.cbl"
flags {"--symbol", "test", "--overrides", path}
go "tests/llvm/load.llvm.cbl"
user_error("Unsupported type in override in file " .. path)
