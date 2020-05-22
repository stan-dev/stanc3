functions {
  vector dz_dt(real t,       // time
               vector z,     // system state {prey, predator}
               real alpha,
               real beta,
               real gamma,
               real delta) {
    real u = z[1];
    real v = z[2];
   
    real du_dt = (alpha - beta * v) * u;
    real dv_dt = (-gamma + delta * u) * v;
   
    return [ du_dt, dv_dt ]';
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
  vector<lower = 0>[2] z_init;  // initial population
  real<lower = 0> sigma[2];   // measurement errors
}
transformed parameters {
  vector[2] z[N]
  = ode_bdf_tol(dz_dt, z_init, 0, ts,
		1e-5, 1e-3, 500,
		alpha, beta, gamma, delta);
  // z = ode_bdf(dz_dt, z_init, 0, ts,
	// 	   alpha, beta, gamma, delta);
  z = ode_rk45_tol(dz_dt, z_init, 0, ts,
		   1e-5, 1e-3, 500,
		   alpha, beta, gamma, delta);
  // z = ode_rk45(dz_dt, z_init, 0, ts,
	// 	   alpha, beta, gamma, delta);
}
model {
  alpha ~ normal(1, 0.5);
  gamma ~ normal(1, 0.5);
  beta ~ normal(0.05, 0.05);
  delta ~ normal(0.05, 0.05);
  sigma ~ lognormal(-1, 1);
  z_init ~ lognormal(log(10), 1);
  for (k in 1:2) {
    y_init[k] ~ lognormal(log(z_init[k]), sigma[k]);
    y[ , k] ~ lognormal(log(z[, k]), sigma[k]);
  }
}