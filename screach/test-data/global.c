#include <stdio.h>

__attribute__((noinline)) void vuln(void) {
  printf("REACHED TARGET\n");
}

int counter = 0;

int process_command(char *buffer) {
  if (buffer == NULL) {
    return 0;
  } else if (buffer[0] == 'I') {
    counter++;
  } else if (buffer[0] == 'D') {
    counter--;
  }
  if (counter == 2) {
    vuln();
  }
  return 1;
}

int make_reachable(char *buffer) {
  counter = 1;
  return process_command(buffer);
}

int main(int argc, char **argv) {
  make_reachable(argv[0]);
}

// CFLAGS: $CFLAGS_COMMON

/// flags {"--entry-symbol", "process_command"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--globals", "symbolic"}
/// go(prog)
/// reached "vuln"

/// flags {"--entry-symbol", "process_command"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--globals", "initialized"}
/// go(prog)
/// failed_to_reach()

/// flags {"--entry-symbol", "make_reachable"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--globals", "symbolic"}
/// go(prog)
/// reached "vuln"

/// flags {"--entry-symbol", "make_reachable"}
/// flags {"--target-symbol", "vuln"}
/// flags {"--globals", "initialized"}
/// go(prog)
/// reached "vuln"
/// check ": 49"
