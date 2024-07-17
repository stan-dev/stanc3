functions {
  vector upper_bound_jacobian(vector x, real ub) {
    jacobian += x;
    return ub - exp(x);
  }
}
transformed data {
  real ub = 1.0;
  int N = 3;
}
parameters {
  @silent real unused;

  @silent vector[N] b_vec_raw;
}
transformed parameters {
  vector[N] bvec = upper_bound_jacobian(b_vec_raw, ub);
}
model {
  unused ~ normal(0, 1);
  b_vec_raw ~ normal(0, 1);
}
