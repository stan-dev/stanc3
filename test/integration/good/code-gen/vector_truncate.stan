data {
  int<lower=0> N;
  real L;
  real<lower=L> U;
  array[N] real<lower=L, upper=U> y;
}
parameters {
  real mu;
  real<lower=0> sigma;
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
}
