
data {
  int N;
  int K;
  matrix[K, N] log_omega;
  matrix[K, K] Gamma;
  vector[K] rho;
}

parameters {
  real y_p;
}

transformed parameters {
  matrix[K, N] log_omega_v;
  matrix[K, K] Gamma_v;
  vector[K] rho_v;
  matrix[K, N + 1] prob;
  prob = hmm_hidden_state_prob(log_omega_v, Gamma, rho);
}

model {
  y_p ~ normal(0, 1);
}
