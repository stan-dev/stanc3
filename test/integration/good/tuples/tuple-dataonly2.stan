functions {
  real tuple_tester(data tuple(array[] real, int) x) {
    return x.1[1];
  }
}
model {
  print(tuple_tester(({1.0}, 2)));
}
