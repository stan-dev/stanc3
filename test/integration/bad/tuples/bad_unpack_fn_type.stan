functions {
  int foo() {
    int x;
    (foo, x) = (foo, 1);
    return x;
  }
}
