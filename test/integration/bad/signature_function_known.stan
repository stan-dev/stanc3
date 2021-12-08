data {
  vector[4] x;
}
parameters {
  vector[4] theta;
}
model {
  target += bernoulli_logit_lpmf(x, theta);
}
