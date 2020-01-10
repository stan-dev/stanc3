data {
  int y_bern[10];
}
parameters {
  real<lower=0, upper=1> p_bern;
  real p_bern_logit;
}
model {
  target += bernoulli_lpmf(y_bern | p_bern);
  target += bernoulli_logit_lpmf(y_bern | p_bern_logit);
}