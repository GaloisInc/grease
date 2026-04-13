-- Test `--debug-cmd`

flags {"--entry-symbol", "test"}
flags {"--target-symbol", "vuln"}
flags {"--debug"}
flags {"--debug-cmd", "help"}
flags {"--debug-cmd", "quit"}
go "../grease-exe/tests/feat/reachability/id.x64.cbl"
check "help (h): Display help text"
