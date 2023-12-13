functions {
  array[] real sho(real t, array[] real y, array[] real theta,
                   array[] real x, array[] int x_int);
  array[] real sho(real t, array[] real y, array[] real theta,
                   array[] real x, array[] int x_int) {
    array[2] real dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
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
  array[5,5] real idxs;
  idxs[1][:] = idxs[1][:];
 }
parameters {
  real x;
  array[3] real theta;
}
model {

  if (b) {
    x ~ normal(0, 1);
  } else {
    x ~ exponential(1);
  }
  while (0.0) {

  }

  int bool = !b < 2 && d || x;

}
generated quantities {
  array[2] real y0 = {1.0, 2.0};
  array[3] real ts = {0.5, 1.0, 2.0};
  array[3,2] real y_hat = integrate_ode(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_45 = integrate_ode_rk45(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_bdf = integrate_ode_bdf(sho, y0, 0.0, ts, theta, x_r, x_i );
  array[3,2] real y_hat_adams = integrate_ode_adams(sho, y0, 0.0, ts, theta, x_r, x_i );
}
