
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

transformed parameters {
  matrix[K, N] log_omega_v;
  matrix[K, K] Gamma_v;
  vector[K] rho_v;
  real density_v;

  density_v = hmm_marginal_lpdf(log_omega | Gamma, rho);
  density_v = hmm_marginal_lpdf(log_omega_v | Gamma, rho);
  density_v = hmm_marginal_lpdf(log_omega | Gamma_v, rho);
  density_v = hmm_marginal_lpdf(log_omega | Gamma, rho_v);
  density_v = hmm_marginal_lpdf(log_omega_v | Gamma_v, rho);
  density_v = hmm_marginal_lpdf(log_omega_v | Gamma, rho_v);
  density_v = hmm_marginal_lpdf(log_omega | Gamma_v, rho_v);
  density_v = hmm_marginal_lpdf(log_omega_v | Gamma_v, rho_v);
}

model {
  y_p ~ normal(0, 1);

  log_omega ~ hmm_marginal(Gamma, rho);
  log_omega ~ hmm_marginal(Gamma_v, rho);
  log_omega ~ hmm_marginal(Gamma, rho_v);
  log_omega ~ hmm_marginal(Gamma_v, rho_v);

  log_omega_v ~ hmm_marginal(Gamma, rho);
  log_omega_v ~ hmm_marginal(Gamma_v, rho);
  log_omega_v ~ hmm_marginal(Gamma, rho_v);
  log_omega_v ~ hmm_marginal(Gamma_v, rho_v);

  target += hmm_marginal_lpdf(log_omega | Gamma, rho);
  target += hmm_marginal_lpdf(log_omega | Gamma_v, rho);
  target += hmm_marginal_lpdf(log_omega | Gamma, rho_v);
  target += hmm_marginal_lpdf(log_omega | Gamma_v, rho_v);
  target += hmm_marginal_lpdf(log_omega_v | Gamma, rho);
  target += hmm_marginal_lpdf(log_omega_v | Gamma_v, rho);
  target += hmm_marginal_lpdf(log_omega_v | Gamma, rho_v);
  target += hmm_marginal_lpdf(log_omega_v | Gamma_v, rho_v);

}
