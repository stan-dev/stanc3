data {
  int N;
  matrix[N, N] X_data;
  vector[N] y_data;
}
parameters {
  real alpha;
  real sigma;
  vector[N] beta;
}
model{
  vector[N] soa_simple= alpha + rep_vector(0.0, N) + beta;
  vector[N] aos_deep = 2 * Phi(y_data / sigma) - 1;
  vector[N] soa_dual_rep = transpose(rep_row_vector(0.0, N)) + rep_vector(sigma, N); 
  vector[N] soa_data_rep = rep_vector(0.0, N) + rep_vector(N, N); 
  vector[N] soa_mix = Phi(y_data / sigma) + soa_simple;
  vector[N] aos_from_data = alpha + sigma * alpha + y_data - alpha - sigma * alpha;
  matrix[N, N] soa_mat_rep = transpose(rep_matrix(2.0, N, N)) + rep_matrix(rep_vector(sigma, N), N); 
  y_data ~ normal(soa_simple, aos_deep);
  target += sum(soa_dual_rep);
  target += sum(aos_from_data);
  target += sum(soa_data_rep);
  target += sum(soa_mix);
  target += sum(soa_mat_rep);
}
