data {
  int K;
  int N_1;
  int N_2;
  array[K] real d_sigma; // bad d_sigma type
  real d_len;
  array[N_1] vector[K] d_vec_1;
  array[N_2] vector[K] d_rvec_1;
}
transformed data {
  matrix[N_1, N_2] transformed_data_matrix;

  transformed_data_matrix = cov_exp_quad(d_vec_1, d_rvec_1, d_sigma, d_len);
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
