data {
  int<lower=1> K;
}
parameters {
  vector[K-1] beta_raw;
}
transformed parameters {
  sum_to_zero_vector[K] beta = sum_to_zero_jacobian(beta_raw);
}
model {
  beta ~ normal(0, inv(sqrt(1 - inv(K))));
}
generated quantities {
  vector[K-1] beta_recovered = sum_to_zero_unconstrain(beta);
  if (max(abs(beta_recovered - beta_raw)) > 1e-10) {
    fatal_error("beta_recovered does not match beta");
  }
}
