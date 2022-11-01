data {
  int<lower=0> N;
  real L;
  real<lower=L> U;
  array[N] real<lower=L, upper=U> y;
}
parameters {
  real mu;
  real<lower=0> sigma;

  vector[N] vector_mu;
  row_vector[N] vector_sigma;
}
model {
  y ~ normal(mu, sigma) T[L, ];

  for (n in 1 : N) {
    y[n] ~ normal(mu, sigma) T[L, ];
  }

  y ~ normal(mu, sigma) T[ , U];

  for (n in 1 : N) {
    y[n] ~ normal(mu, sigma) T[ , U];
  }

  y ~ normal(mu, sigma) T[L, U];

  for (n in 1 : N) {
    y[n] ~ normal(mu, sigma) T[L, U];
  }

  // Vector mu

  y ~ normal(vector_mu, sigma) T[L, ];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], sigma) T[L, ];
  }

  y ~ normal(vector_mu, sigma) T[ , U];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], sigma) T[ , U];
  }

  y ~ normal(vector_mu, sigma) T[L, U];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], sigma) T[L, U];
  }

  // vector sigma

  y ~ normal(mu, vector_sigma) T[L, ];

  for (n in 1 : N) {
    y[n] ~ normal(mu, vector_sigma[n]) T[L, ];
  }

  y ~ normal(mu, vector_sigma) T[ , U];

  for (n in 1 : N) {
    y[n] ~ normal(mu, vector_sigma[n]) T[ , U];
  }

  y ~ normal(mu, vector_sigma) T[L, U];

  for (n in 1 : N) {
    y[n] ~ normal(mu, vector_sigma[n]) T[L, U];
  }

  // vector both

  y ~ normal(vector_mu, vector_sigma) T[L, ];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], vector_sigma[n]) T[L, ];
  }

  y ~ normal(vector_mu, vector_sigma) T[ , U];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], vector_sigma[n]) T[ , U];
  }

  // also test non-variable expression
  y ~ normal(vector_mu * 4, vector_sigma + 1) T[L, U];

  for (n in 1 : N) {
    y[n] ~ normal(vector_mu[n], vector_sigma[n]) T[L, U];
  }
}
