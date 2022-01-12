functions {
  real[] sho(real t, real[] y, real[] theta, real[] x, int[] x_int) {
    real dydt[2];
    dydt[1] <- y[2];
    dydt[2] <- -y[1] - theta[1] * y[2];
    return dydt;
  }
  real normal_log_log(real a, real b, real c) {
    return (a-b)/c;
  }
  real foo_cdf_log(real x, real y) {
    real s = 0;
    for(i in 1:10) {
      s += if_else(x<0, multiply_log(1, y), 0);
    }
    return s;
  }
  real foo_cdf(real a, real b, real c) {
    return a + b + c;
  }
}
data {
  int<lower=1> N;
  array[N] real x_quad;
}
transformed data {
  int a = -12;
  real b = 1.5;
  int c = abs(a);
  real d = abs(b);
  array[0] int x_i;
  array[0] real x_r;
  matrix[N, N] K = cov_exp_quad(x_quad, 1.0, 1.0);
  real idxs[5,5];
  idxs[1][:] = idxs[1][:];
  idxs[:][1] = idxs[:][2];
 }
parameters {
  real x;
  array[3] real theta;
}
model {
  real k = if_else(b<0, multiply_log(1, d), 0);
  target += binomial_coefficient_log(10, 10);

  c ~ poisson_lpmf(3.0);
  c ~ poisson_log(3.0);
  c ~ poisson_log_log(3.0);
  if (a) {
    x ~ normal(0, 1);
    x ~ normal_log(0, 1);
    x ~ normal_log_log(0, 1);
    target += normal_log_log(x,1,2);
    increment_log_prob(std_normal_lpdf(x));
  } else {
    x ~ exponential(1);
    x ~ exponential_log(1);
    increment_log_prob(foo_cdf_log(x, 1));
  }

  target += normal_log(x, 0, 1)
    + normal_cdf_log(2, 0, 1)
    + normal_ccdf_log(3, 0, 1);
  target += sum(K);

  target += normal_cdf(1, 2, 3);
  target += normal_cdf(normal_cdf(0, 1, 2), 2, 3);
  target += foo_cdf(1, 2, 3);

  target += hypergeometric_log(1, 2, 3, 4);
  target += gaussian_dlm_obs_log(K,K,K,K,K,K[:,1],K);

  print("target: ", get_lp());
}
generated quantities {
  array[2] real y0 = {1.0, 2.0};
  array[3] real ts = {0.5, 1.0, 2.0};
  array[3,2] real y_hat = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_adams = integrate_ode_adams(sho, y0, 0.0, ts, theta, x_r, x_i );
}
