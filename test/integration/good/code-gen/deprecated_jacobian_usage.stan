// existing code should continue to work

functions {


  real bar_jacobian(real x) {
    return x;
  }

  real bar(real x) {
    return bar_jacobian(x);
  }
}
transformed parameters {

}

generated quantities {
  real b_jacobian = bar(10);
}
