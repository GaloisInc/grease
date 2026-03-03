
// Checks that avoid addr forces the
// execution down the longer path by checking that we
// dont execute the closest instruction

void target() {}

int longfunc(int n) {
  int z = 0;
  for (int i = 0; i < n; i++) {
    z += 2;
  }

  return z;
}

void go(int n) {
  if (n == 1) {
    // fastest path but we are going to avoid
    n++;
    target();
  } else {
    n = longfunc(n);
    target();
  }
}

int main(int argc, char **argv) {
  go(argc);
  return 0;
}

/// flags {"--entry-symbol", "go"}
/// flags {"--target-symbol", "target"}
/// flags {"--explore"}
/// flags {"--callgraph", "./directed-test-data/avoid-addr.tsv"}
/// flags {"--avoid-addr", "0x401111"}
/// flags {"-v"}
/// go(prog)
/// check_not(execute_insn_string(0x401111))
/// check(execute_insn_string(0x40111c))
/// reached "target"