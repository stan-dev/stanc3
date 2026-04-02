functions {
  real my_upper_bound_jacobian(real x, real ub) {
    jacobian += x;
    return ub - exp(x);
  }
}

data {
  real beta_raw;
  real ub;
}

transformed parameters {
  real beta = upper_bound_jacobian(beta_raw, ub);
  real beta2 = my_upper_bound_jacobian(beta_raw, ub);
}
