
void target() {}
// specially named overrides that signal to the test expected distance
// comparisons
void short_path() {}
void long_path() {}

int bar(int y) { return y + 3; }

int foo(int x) {
  if (x > 1) {
    long_path();
    bar(x);
  } else {
    short_path();
  }

  target();
  return x;
}
