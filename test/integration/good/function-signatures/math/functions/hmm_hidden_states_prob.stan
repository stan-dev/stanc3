data {
  int N;
  int K;
  matrix[K, N] log_omega;
  matrix[K, K] Gamma;
  vector[K] rho;
}

transformed data {
  matrix[K, N] prob = hmm_hidden_state_prob(log_omega, Gamma, rho);
}

parameters {
  real y_p;
}

transformed parameters {
  matrix[K, N] log_omega_v;
  matrix[K, K] Gamma_v;
  vector[K] rho_v;
}

model {
  y_p ~ normal(0,1);
}

generated quantities {
  matrix[K, N] prob_gen;

  prob_gen = hmm_hidden_state_prob(log_omega, Gamma, rho);
  prob_gen = hmm_hidden_state_prob(log_omega_v, Gamma, rho);
  prob_gen = hmm_hidden_state_prob(log_omega, Gamma_v, rho);
  prob_gen = hmm_hidden_state_prob(log_omega, Gamma, rho_v);
  prob_gen = hmm_hidden_state_prob(log_omega_v, Gamma_v, rho);
  prob_gen = hmm_hidden_state_prob(log_omega_v, Gamma, rho_v);
  prob_gen = hmm_hidden_state_prob(log_omega, Gamma_v, rho_v);
  prob_gen = hmm_hidden_state_prob(log_omega_v, Gamma_v, rho_v);
}
