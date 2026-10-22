/* $ stanc --auto-format ../target.stan : [exit 0] */
parameters {
  real<lower=0> y;
}
transformed parameters {
  print("target = ", target());
}
model {
  print("target = ", target());
  y ~ normal(0, 1);
}

