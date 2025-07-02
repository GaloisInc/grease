/* Copyright (c) Galois, Inc. 2025 */

// Tests that heuristics are unneeded when using dwarf populated shapes to
// create a memory precondition that allows sum_list to execute.
// Tests that the recursive type instantiation is bounded

struct tree;
struct treenode {
  struct tree *lhs;
  struct tree *rhs;
};

struct tree {
  int x;
  struct treenode *next;
};

// getting this to only be safe when balanced is a bit annoying
// we always assume init by one
int sum_tree(struct tree *in) {
  int val = in->x;
  if (in->next) {
    val += sum_tree(in->next->lhs);
    val += sum_tree(in->next->rhs);
  }

  return val;
}

// Each test checks where the null pointer was placed

// all: flags {"--symbol", "sum_tree", "--enable-dwarf-preconditions", "--no-heuristics"} 
// x64: go(prog)
