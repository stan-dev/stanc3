// existing code should continue to work

functions {
  real foo(real x) {
    real jacobian = 0;
    jacobian += x;
    return jacobian;
  }
}
transformed parameters {
  real jacobian = 1;
  jacobian += foo(1);
}
