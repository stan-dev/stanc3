functions {


  real bar_jacobian(real x) {
    // even though this doesn't actually touch the jacobian, scoping should prevent it
    // being called in a normal function
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
