functions {
  int foo() {
    tuple(real, int) x;
    x = (foo, 1);
    return x.2;
  }
}
