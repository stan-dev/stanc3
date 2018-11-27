  $ $TESTDIR//../../_build/default/stanc.exe --auto-format "$TESTDIR//ode_good.stan"
  functions {
    real[] harm_osc_ode(real t, real[] y, real[] theta, real[] x, int[] x_int) {
      real dydt[2];
      dydt[1] <- x[1] * y[2];
      dydt[2] <- -y[1] - theta[1] * y[2];
      return dydt;
    }
  }
  data {
    real y0[2];
    real t0;
    real ts[10];
    real x[1];
    int x_int[0];
    real y[10, 2];
  }
  parameters {
    real theta[1];
    real<lower=0> sigma;
  }
  transformed parameters {
    real y_hat[10, 2];
    y_hat <- integrate_ode(harm_osc_ode, y0, t0, ts, theta, x, x_int);
    y_hat <- integrate_ode_rk45(harm_osc_ode, y0, t0, ts, theta, x, x_int);
    y_hat <- integrate_ode_bdf(harm_osc_ode, y0, t0, ts, theta, x, x_int);
    y_hat <- integrate_ode_adams(harm_osc_ode, y0, t0, ts, theta, x, x_int);
    y_hat <- integrate_ode_rk45(harm_osc_ode, y0, t0, ts, theta, x, x_int, 0.01, 0.01, 10);
    y_hat <- integrate_ode_bdf(harm_osc_ode, y0, t0, ts, theta, x, x_int, 0.01, 0.01, 10);
    y_hat <- integrate_ode_adams(harm_osc_ode, y0, t0, ts, theta, x, x_int, 0.01, 0.01, 10);
  }
  model {
    for (t in 1 : 10) y[t] ~ normal(y_hat[t], sigma);
  }
  

