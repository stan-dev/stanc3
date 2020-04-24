functions {
  real[] dz_dt(real t,       // time
               real[] z,     // system state {prey, predator}
               real alpha,
               real beta) {
    real u = z[1];
    real v = z[2];
   
    real du_dt = (alpha * v) * u;
    real dv_dt = (beta * u) * v;
   
    return { du_dt, dv_dt };
  }
}
data {
  int<lower = 0> N;          // number of measurement times
  real ts[N];                // measurement times > 0
  real y_init[2];            // initial measured populations
  real<lower = 0> y[N, 2];   // measured populations
  real a[2];
}
parameters {
  real<lower = 0> alpha;
  real<lower = 0> beta;
  real<lower = 0> z_init[2];  // initial population
  real<lower = 0> sigma[2];   // measurement errors
}
transformed parameters {
  real z[N, 2]
  = ode_bdf_tol(dz_dt, z_init, 0.0, ts,
            1e-5, a, 500,
            alpha, beta);
}
model {
}
