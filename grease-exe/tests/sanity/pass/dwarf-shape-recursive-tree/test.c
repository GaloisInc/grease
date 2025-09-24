/* Copyright (c) Galois, Inc. 2025 */

// Tests that heuristics are unneeded when using dwarf populated shapes
// to initialize a tree. The tree has an implicit invariant that if next != null then both 
// lhs and rhs are initialized.

struct tree;
struct treenode {
  struct tree *lhs;
  struct tree *rhs;
};

struct tree {
  int x;
  struct treenode *next;
};

// Getting this to only be safe when balanced is a bit annoying.
// A normal person would write this code to just check in != NULL but then 
// this code would work with a precondition that is unbalanced, so we instead check the child to make sure that implicitly 
// lhs and rhs must be initialized together. 
int sum_tree(struct tree *in) {
  int val = in->x;
  if (in->next) {
    val += sum_tree(in->next->lhs);
    val += sum_tree(in->next->rhs);
  }

  return val;
}

// Checks that we have multiple unrollings both of the lhs and rhs

// all: flags {"--symbol", "sum_tree", "--use-debug-info-types", "--no-heuristics"} 
// x64: go(prog)
// x64: check "00002b: XX XX XX XX ## ## ## ## 00002c+0000000000000000"
// x64: check "00002c: 00002d+0000000000000000 00002e+0000000000000000"
// x64: check "00002d: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00"
// x64: check "00002e: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00"

