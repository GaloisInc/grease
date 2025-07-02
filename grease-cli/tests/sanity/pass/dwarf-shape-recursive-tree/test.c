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


// x64: precond=[[000031: XX XX XX XX ## ## ## ## 000032+0000000000000000
// x64:000032: 000033+0000000000000000 000034+0000000000000000
// x64:000033: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00
// x64:000034: XX XX XX XX ## ## ## ## 00 00 00 00 00 00 00 00]]

// all: flags {"--symbol", "sum_tree", "--enable-dwarf-preconditions", "--no-heuristics"} 
// x64: go(prog)
// x64: check(precond)