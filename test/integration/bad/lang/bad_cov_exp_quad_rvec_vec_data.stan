data {
  real d_sigma;
  real d_len;
  int K;
  int N_1;
  int N_2;
  array[N_1] vector[K] d_vec_1;
  array[N_2] row_vector[K] d_rvec_1; // bad mixed Eigen vector types
}
transformed data {
  matrix[N_2, N_1] transformed_data_matrix;

  transformed_data_matrix = cov_exp_quad(d_rvec_1, d_vec_1, d_sigma, d_len);
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
