
data {
  int N;
  int K;
  matrix[K, N] log_omega;
  matrix[K, K] Gamma;
  vector[K] rho;
}

transformed data {
  real density = hmm_marginal_lpdf(log_omega | Gamma, rho);
}

parameters {
  real y_p;
}

model {
  y_p ~ normal(0, 1);
}
