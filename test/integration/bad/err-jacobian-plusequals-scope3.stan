functions {
  real upper_bound_jacobian(real x, real ub) {
    jacobian += x;
    return ub - exp(x);
  }
}

parameters {
  real b_raw;
  real ub;
}

model {
  real b = upper_bound_jacobian(b_raw, ub);
}
