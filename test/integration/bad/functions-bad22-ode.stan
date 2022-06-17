functions {
  array[] real sho(real t,
             array[] real y,
             array[] real theta,
             array[] real x_r,
             array[] int x_i) {
    array[2] real dydt;
    dydt[1] = y[2];
    dydt[2] = -y[1] - theta[1] * y[2];
    return dydt;
  }

  array[,] real do_integration_nested(array[] real y0, real t0, data array[] real ts, array[] real theta, matrix xmat_r) {
    array[0] int x_i;
    return(integrate_ode_rk45(sho, y0, t0, ts, theta, to_array_1d(xmat_r[1]), x_i));
  }

  array[,] real do_integration(array[] real y0, real t0, array[] real ts, array[] real theta, matrix xmat_r, array[] real x_r) {
    matrix[2,2] xmat_sub_r;
    xmat_sub_r = block(xmat_r, 1, 1, 2, 2);
    return(do_integration_nested(y0, t0, ts, theta, xmat_sub_r));
  }
}
data {
  int<lower=1> T;
  array[T,2] real y;
  real t0;
  array[T] real ts;
}
transformed data {
  array[1] real x_r;
  matrix[2,2] xmat_r;
}
parameters {
  array[2] real y0;
  vector<lower=0>[2] sigma;
  array[1] real theta;
}
transformed parameters {
  array[T,2] real y_hat;
  y_hat = do_integration(y0, t0, ts, theta, xmat_r, x_r);
}
model {
  sigma ~ cauchy(0, 2.5);
  theta ~ normal(0, 1);
  y0 ~ normal(0, 1);
  for (t in 1:T)
    y[t] ~ normal(y_hat[t], sigma);
}
