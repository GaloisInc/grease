/* Copyright (c) Galois, Inc. 2025 */

// Tests that heuristics are unneeded when using dwarf populated shapes to 
// create a memory precondition that allows sum_list to execute.
// Tests that the recursive type instantiation is bounded 


struct llnode {
  int x;
  struct llnode *next;
};

int sum_list(struct llnode *in) {
  struct llnode *curr = in;
  int sum = 0;
  while (curr) {
    sum += curr->x;
    curr = curr->next;
  }
  return sum;
}

// Each test checks where the null pointer was placed

// all: flags {"--symbol", "sum_list", "--enable-dwarf-preconditions", "--no-heuristics"}
// x64: go(prog) 
// x64: check("00000b: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00")

// all: flags {"--symbol", "sum_list", "--enable-dwarf-preconditions", "--no-heuristics", "--type-unrolling-bound", "12"}
// x64: go(prog) 
// x64: ok()
// x64: check("000014: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00")

// all: flags {"--symbol", "sum_list", "--enable-dwarf-preconditions", "--no-heuristics", "--type-unrolling-bound", "0"}
// x64: go(prog) 
// x64: ok()
// check("000008: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00")