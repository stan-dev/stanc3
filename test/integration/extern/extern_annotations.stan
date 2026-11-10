// can't live in test/integration/good, because CI will try to compile to an executable.
functions {
  @extern real foo(int x, int y);
  @extern void bar(int x, int y, int z, int w, int a, int b, int d, int e, int f);
}
