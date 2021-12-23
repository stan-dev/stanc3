parameters {
  array[2] real<lower=-10, upper=10> y;
}
transformed parameters {
  array[2] real<lower=0> z;
  z[1] = exp(y[1]);
  z[2] = exp(y[2]) * exp(y[1]);
}
model {
  y ~ normal(0, 1);
}
generated quantities {
  int xgq;
  xgq = 2713;
}

