functions {
  void foo(tuple(real, real) x) {
    print(x.1);
  }
}

model {
  foo((1, 2, 3));
}
