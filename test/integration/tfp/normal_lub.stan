data {
  vector[10] y_lub;
  vector[10] y_ub;
  vector[10] y_lb;
}
parameters {
  real<lower=-3,upper=3> theta_lub;
  real<upper=1> theta_ub;
  real<lower=0> theta_lb;
}
model {
  theta_lub ~ normal(0,5);
  theta_ub ~ normal(0,5);
  theta_lb ~ normal(0,5);
  y_lub ~ normal(theta_lub,1);
  y_ub ~ normal(theta_ub,1);
  y_lb ~ normal(theta_lb,1);
}
