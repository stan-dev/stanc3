transformed data {
  int N = 2;
  int M = 3;
  int K = N * M;
}

parameters {
  real<lower=-2.0, upper=2.0> J;
  real d_real_1d_ar[N];
  real d_real_3d_ar[N,M,K];
  vector[N] d_vec;
  vector[N] d_1d_vec[N];
  vector[N] d_3d_vec[N,M,K];
  row_vector[N] d_row_vec;
  row_vector[N] d_1d_row_vec[N];
  row_vector[N] d_3d_row_vec[N,M,K];
  matrix<lower=0,upper=1>[2,3] d_ar_mat[4,5];
  simplex[N] d_simplex;
  simplex[N] d_1d_simplex[N];
  simplex[N] d_3d_simplex[N,M,K];
  cholesky_factor_cov[5,4] d_cfcov_54;
  cholesky_factor_cov[3] d_cfcov_33;
  cholesky_factor_cov[3] d_cfcov_33_ar[K];
}
