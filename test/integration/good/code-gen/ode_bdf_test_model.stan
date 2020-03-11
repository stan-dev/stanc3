functions {
  real[] dz_dt(real t,       // time
               real[] z,     // system state {prey, predator}
               real alpha,
               real beta,
               real gamma,
               real delta) {
    real u = z[1];
    real v = z[2];
   
    real du_dt = (alpha - beta * v) * u;
    real dv_dt = (-gamma + delta * u) * v;
   
    return { du_dt, dv_dt };
  }
}
data {
  int<lower = 0> N;          // number of measurement times
  real ts[N];                // measurement times > 0
  real y_init[2];            // initial measured populations
  real<lower = 0> y[N, 2];   // measured populations
}
parameters {
  real<lower = 0> alpha;
  real<lower = 0> beta;
  real<lower = 0> gamma;
  real<lower = 0> delta;
  real<lower = 0> z_init[2];  // initial population
  real<lower = 0> sigma[2];   // measurement errors
}
transformed parameters {
  real z[N, 2]
  = ode_bdf(dz_dt, z_init, 0, ts,
            1e-5, 1e-3, 5e2,
            alpha, beta, gamma, delta);
}
model {
  alpha ~ normal(1, 0.5);
  gamma ~ normal(1, 0.5);
  beta ~ normal(0.05, 0.05);
  delta ~ normal(0.05, 0.05);
  sigma ~ lognormal(-1, 1);
  z_init ~ lognormal(10, 1);
  for (k in 1:2) {
    y_init[k] ~ lognormal(log(z_init[k]), sigma[k]);
    y[ , k] ~ lognormal(log(z[, k]), sigma[k]);
  }
}