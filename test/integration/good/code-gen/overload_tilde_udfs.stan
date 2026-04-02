functions {
  real distribution_lpdf(vector y, real mu, real sigma) {
    return normal_lpdf(y | mu, sigma);
  }
  
  real distribution_lpmf(array[] int y, real pi) {
    return bernoulli_lpmf(y | pi);
  }
}
data {
  int N;
  vector[N] y;
}
parameters {
  real mu;
  real<lower=0> sigma;
}
model {
  target += distribution_lpdf(y | mu, sigma); // works fine
  y ~ distribution(mu, sigma); // syntax error
}
