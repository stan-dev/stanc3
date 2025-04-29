// existing code should continue to work

functions {
  real foo(real x) {
    real jacobian = 0;
    jacobian += x;
    return jacobian;
  }

  real bar_jacobian(real x) {
    return x;
  }

  real bar(real x) {
    return bar_jacobian(x);
  }
}
transformed parameters {
  real jacobian = 1;
  jacobian += foo(1);
}

generated quantities {
  real b_jacobian = bar(10);
}
