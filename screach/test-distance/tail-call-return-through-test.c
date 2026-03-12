
void target() {}
void short_path() {}

int baz(int);
int bar(int);

int baz(int y) { return y + 1; }

int bar(int y) {
   __attribute__((musttail)) return baz(y);
}

void long_path() { bar(1); }

int foo(int x) {
  if (x > 1) {
    long_path();
  } else {
    short_path();
  }

  target();
  return x;
}
