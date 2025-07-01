
// hacky linked list to keep this simple
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

// all: flags {"--symbol", "foo", "--enable-dwarf-preconditions",
// "--no-heuristics"} x64: go(prog) x64: ok()