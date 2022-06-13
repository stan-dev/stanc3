transformed data {
  real a;
  a = inv_inc_beta(1, 1, 0.4);
  a = inc_beta(1, 1, 0.3);
  a = inc_beta(0, 3, 1);
  a = inc_beta(1, 2.7, 0);
  a = inc_beta(1, 0, 1);
  a = inc_beta(2.7, 0, 0.8);
}  
parameters {
  real<lower=0, upper=1> p;
  real<lower=0> b;
}
transformed parameters {
  real c;
  c = inc_beta(b, b, p);
  c = inc_beta(p, p, p);
  c = inc_beta(0, b, p);
  c = inc_beta(b, p, 0);
  c = inc_beta(p, b, p);
}
model {
  b ~ normal(0, 1);
}