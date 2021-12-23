transformed data {
  array[2] real y;
  real mu0;
  real sigma;
  
  y[1] = 1.6;
  y[2] = 1.4;
  
  mu0 = 1.5;
  sigma = 1.0;
}
parameters {
  real<lower=0> mu;
}
model {
  mu ~ normal(mu0, sigma);
  
  for (n in 1 : 2) 
    y[n] ~ normal(mu, 1.0);
}

