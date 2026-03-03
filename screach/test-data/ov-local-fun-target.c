void call_vuln(void) {}

void test(void) {
  call_vuln();
}

// CFLAGS: $CFLAGS_COMMON
// CFLAGS: $CFLAGS_NO_LIBS
// CFLAGS: $CFLAGS_STATIC

/// flags {"--entry-symbol", "test"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--overrides", "test-data/extra/call_vuln.cbl"}
/// go(prog)
/// reached "vuln"
