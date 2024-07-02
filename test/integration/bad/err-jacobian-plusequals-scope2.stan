functions {
  real upper_bound(real x, real ub) {
    jacobian += x;
    return ub - exp(x);
  }
}

parameters {
  real b_raw;
  real ub;
}

transformed parameters {
  real b = upper_bound(b_raw, ub);
}
