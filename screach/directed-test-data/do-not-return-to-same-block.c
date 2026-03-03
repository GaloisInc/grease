// This is a regression test to catch a bug where we would return to the start
// of the block a call. This bug meant that
// the distance tracing would reach a call twice (the second time the function
// is entered it ends up treated as recursive so the distance tracing terminated
// but with double the expected count) effectively meant we would double count
// the distance of a function call.

void target() {}

// the idea with this function is that it is fastest to simply return 
int longfunc(int n) {
  int z = 0;
  if (n == 1) {
    for (int i = 0; i < n; i++) {
      z += 2;
    }
  }
  return z;
}

void go(int n) {
  n = longfunc(n);
  target();
}

int main(int argc, char **argv) {
  go(argc);
  return 0;
}


// first we check that we do start computing longfunc
// then we check that we return from longfunc, 
// after we return from longfunc it should not be the case
// that we get another check of longfunc's distance

/// flags {"--entry-symbol", "go"}
/// flags {"--target-symbol", "target"}
/// flags {"--explore"}
/// flags {"--callgraph", "./directed-test-data/do-not-return-to-same-block.tsv"}
/// flags {"-v"}
/// go(prog)
/// check "computing min dist for cfg: ProgramLoc {plFunction = longfunc, plSourceLoc = 0x4010d4}"
/// check "handling return"
// check that after returning we dont get recursion into longfunc. We check that we terminate the first dist of longfunc
// before starting again
/// not_until("done computed min dist for cfg: ProgramLoc {plFunction = longfunc, plSourceLoc = 0x4010d4}", "computing min dist for cfg: ProgramLoc {plFunction = longfunc")

/// reached "target"