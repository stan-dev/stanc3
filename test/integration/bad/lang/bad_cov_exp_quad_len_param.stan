data {
  int K;
  int N_1;
  int N_2;
  array[N_1] vector[K] d_vec_1;
  array[N_2] vector[K] d_rvec_1;
}
parameters {
  array[K] real d_len; // bad d_len type
  real d_sigma;
}
transformed parameters {
  matrix[N_1, N_2] transformed_param_matrix;

  transformed_param_matrix = cov_exp_quad(d_vec_1, d_rvec_1, d_sigma, d_len);
}
parameters {
  real y;
}
model {
  y ~ normal(0,1);
}
