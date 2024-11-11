functions {
  real jacobian(real x, int y) {
    return x + y;
  }
  void jacobian(real x, real y) {
    print(x + y);
  }
}
transformed data {
  print(jacobian(1.0, 2));
  jacobian(1.0, 2.0);
}
parameters {
  real jacobian;
}
model {
  if (1 < 2 < 3 < 4) {
    // multiple comparisons
  }
}
