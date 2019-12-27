parameters {
  real<lower=0, upper=1> p;
}
model {
  target+= beta_lpdf(p|1.5,0.8);
}
