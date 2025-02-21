data {
  real beta_raw;
  real ub;
}

transformed parameters {
  real beta = upper_bound_jacobian(beta_raw, ub);
}
