functions {
  real[] sho(real t,real[] y, real[] theta, real[] x, int[] x_int) {
    real dydt[2];
    dydt[1] <- y[2];
    dydt[2] <- -y[1] - theta[1] * y[2];
    return dydt;
  }
}
transformed data {
  int a = -12;
  real b = 1.5;
  int c = abs(a);
  real d = abs(b);
  int x_i[0];
  real x_r[0];
}
parameters {
  real x;
  real theta[3];
}
model {
  real k = if_else(b<0, multiply_log(1, d), 0);
  target += binomial_coefficient_log(10, 10);

  c ~ poisson_lpmf(3.0);
  c ~ poisson_log(3.0);
  x ~ normal_log(0, 1);
  increment_log_prob(std_normal_lpdf(x));

  target += normal_log(x, 0, 1)
    + normal_cdf_log(2, 0, 1)
    + normal_ccdf_log(3, 0, 1);

  print("target: ", get_lp());
}
generated quantities {
  real y0[2] = {1.0, 2.0};
  real ts[3] = {0.5, 1.0, 2.0};
  real y_hat[3,2] = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i );
}
