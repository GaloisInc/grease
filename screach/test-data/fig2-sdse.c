// Example adapted from DSE paper

#include <snapshot.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void something() { return; }

void expensive() {
  expensive(); // infinite recursion
}

void vuln() { puts("oh no, vuln() reached!"); }

int go(int argc, char *argv[]) {
  int i = 0;
  int n = 0;
  int b[4] = {0, 0, 0, 0};
  for (i = 0; i < argc; i++) {
    if (*argv[i] == 'b') {
      if (n >= 4) {
        vuln(); // paper uses assert
      }
      b[n++] = 1; // potential buffer overflow
    } else {
      expensive();
    }
  }

  while (1) {
    if (getchar()) {
      something();
    }
  }
}

int main(int argc, char *argv[]) {
  if (argc == 1) {
    return ecfs_on_startup();
  }

  go(argc, argv);
}

/// flags {"--entry-symbol", "go"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--explore"}
/// flags {"--callgraph", "./test-data/fig2-sdse-cg.csv"}
/// flags {"--target-containing-function-address", "0x10002530"}
/// go(prog)
/// reached "vuln"
/// verified()
