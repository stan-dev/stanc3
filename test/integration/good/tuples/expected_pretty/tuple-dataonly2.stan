/* $ stanc --auto-format ../tuple-dataonly2.stan : [exit 0] */
functions {
  real tuple_tester(data tuple(array[] real, int) x) {
    return x.1[1];
  }
}
model {
  print(tuple_tester(({1.0}, 2)));
}

