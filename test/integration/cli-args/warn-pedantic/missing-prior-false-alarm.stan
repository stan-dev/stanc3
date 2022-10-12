data {
  int<lower=0> N;
  array[N] int<lower=0,upper=1> y; // or int<lower=0,upper=1> y[N];
}
transformed data {
  real prior_1 = 1;
}
parameters {
  real<lower=0,upper=1> theta;
}
model {
  theta ~ beta(prior_1,1);  // uniform prior on interval 0,1
  y ~ bernoulli(theta);
}
