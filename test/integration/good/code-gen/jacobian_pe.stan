functions {
  real upper_bound_jacobian(real x, real ub) {
    jacobian += x;
    return ub - exp(x);
  }
  vector upper_bound_jacobian(vector x, real ub) {
    vector[size(x)] result;
    for (n in 1:size(x)) {
      result[n] = upper_bound_jacobian(x[n], ub);
    }
    return result;
  }
}
data {
  real ub;
  int N;
}
parameters {
  real b_raw;
  vector[N] b_vec_raw;
  real b_direct_raw;
}
transformed parameters {
  real b = upper_bound_jacobian(b_raw, ub);
  vector[N] bvec = upper_bound_jacobian(b_vec_raw, ub);

  jacobian += b_direct_raw;
  real b_direct = ub - exp(b_direct_raw);
}

