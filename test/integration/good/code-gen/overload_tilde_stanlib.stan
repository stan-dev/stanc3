functions {
  real bernoulli_lpmf(int y, real theta, real alpha) {
    return log(theta);
  }
}
data {
  int y;
}
parameters {
  real<lower=0, upper=1> theta;
}
model {
  y ~ bernoulli(theta);
}
