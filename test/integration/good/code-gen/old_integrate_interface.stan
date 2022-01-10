functions {
  array[] real dz_dt(real t, // time
                     array[] real z,
                     // system state {prey, predator}
                     array[] real theta, // parameters
                     array[] real x_r, // unused data
                     array[] int x_i) {
    real u = z[1];
    real v = z[2];
    
    real alpha = theta[1];
    real beta = theta[2];
    real gamma = theta[3];
    real delta = theta[4];
    
    real du_dt = (alpha - beta * v) * u;
    real dv_dt = (-gamma + delta * u) * v;
    return {du_dt, dv_dt};
  }
}
data {
  int<lower=0> N; // number of measurement times
  array[N] real ts; // measurement times > 0
  array[2] real y_init; // initial measured populations
  array[N, 2] real<lower=0> y; // measured populations
}
parameters {
  real<lower=0> alpha;
  real<lower=0> beta;
  real<lower=0> gamma;
  real<lower=0> delta;
  array[2] real<lower=0> z_init; // initial population
  array[2] real<lower=0> sigma; // measurement errors
}
transformed parameters {
  array[N, 2] real z = integrate_ode_bdf(dz_dt, z_init, 0, ts,
                                         {alpha, beta, gamma, delta},
                                         rep_array(0.0, 0), rep_array(
                                         0, 0), 1e-5, 1e-3, 5e2);
}
model {
  alpha ~ normal(1, 0.5);
  gamma ~ normal(1, 0.5);
  beta ~ normal(0.05, 0.05);
  delta ~ normal(0.05, 0.05);
  sigma ~ lognormal(-1, 1);
  z_init ~ lognormal(log(10), 1);
  for (k in 1 : 2) {
    y_init[k] ~ lognormal(log(z_init[k]), sigma[k]);
    y[ : , k] ~ lognormal(log(z[ : , k]), sigma[k]);
  }
}

