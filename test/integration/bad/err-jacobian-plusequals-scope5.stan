
parameters {
  real b_raw;
  real ub;
}

model {
  real b = upper_bound_jacobian(b_raw, ub);
}
