
data {
  vector[3] yy0;
  vector[3] yp0;
  real t0;
  array[1] real x;
  array[4] vector[3] y;
}
transformed data {
  array[4] real ts;
}
parameters {
  array[3] real theta;
  vector[3] yy0_var;
  vector[3] yp0_var;
  real<lower=0> sigma;
}
transformed parameters {
  array[4] vector[3] y_hat;
  y_hat = dae(0.0, yy0, yp0, t0, ts, theta, x);
}
model {
  for (t in 1 : 4)
    y[t] ~ normal(y_hat[t], sigma); // independent normal noise
}
