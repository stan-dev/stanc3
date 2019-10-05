data {
  int<lower=1> k;
  int<lower=0> n;
  matrix[n, k] X;
  vector[n] y;
}

parameters {
  vector[k] beta;
  real<lower=0> sigma;
  real alpha;
}

model {
  target += normal_id_glm_lpdf(y | X, alpha, beta, sigma);
}
