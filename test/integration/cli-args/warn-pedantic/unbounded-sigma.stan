parameters {
  real x;
  real sigma_a;
  real<lower=0> sigma_b;
  real<lower=-1, upper=1> sigma_c;
  real<lower=0, upper=1> sigma_d;
  real<lower=1, upper=2> sigma_e;
}
model {
  x ~ normal (0, sigma_a);
  x ~ normal (0, sigma_b);
  x ~ normal (0, sigma_c);
  x ~ normal (0, sigma_d);
  x ~ normal (0, sigma_e);
  real z = 1 - 2;
  x ~ normal (0, z);
}
