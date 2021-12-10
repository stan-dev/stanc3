data {
  int<lower=1> N;
  array[N] real x;
  vector[N] y;
}
transformed data {
  real sum_y;
  profile("sum") {
    sum_y = sum(y);
  }
}
parameters {
  real<lower=0> rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
}
model {
  matrix[N, N] cov;
  matrix[N, N] L_cov;
  profile("cov_exp_quad") {
    cov = gp_exp_quad_cov(x, alpha, rho) + diag_matrix(rep_vector(sigma, N));
  }
  profile("cholesky_decompose") {
    L_cov = cholesky_decompose(cov);
  }
  profile("likelihood") {
    profile("priors") {
      rho ~ gamma(25, 4);
      alpha ~ normal(0, 2);
      sigma ~ normal(0, 1);
    }
    profile("multi_normal_cholesky") {
      y ~ multi_normal_cholesky(rep_vector(0, N), L_cov);
    }
  }
}

