functions {
  vector chem_dae(real t, vector yy, vector yp,
                        array[] real p, array[] real x, int unused1) {
    vector[3] res;
    res[1] = yp[1] + p[1] * yy[1] - p[2] * yy[2] * yy[3];
    res[2] = yp[2] - p[1] * yy[1] + p[2] * yy[2] * yy[3] + p[3] * yy[2] * yy[2];
    res[3] = yy[1] + yy[2] + yy[3] - 1.0;
    return res;
  }
    vector chem_dae(real t, vector yy, vector yp,
                        array[] real p, array[] real x) {
    vector[3] res;
    res[1] = yp[1] + p[1] * yy[1] - p[2] * yy[2] * yy[3];
    res[2] = yp[2] - p[1] * yy[1] + p[2] * yy[2] * yy[3] + p[3] * yy[2] * yy[2];
    res[3] = yy[1] + yy[2] + yy[3] - 1.0;
    return res;
  }
}
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
  {
    y_hat = dae(chem_dae, yy0, yp0, t0, ts, theta, x);
  }
  {
    y_hat = dae(chem_dae, yy0, yp0, t0, ts, theta, x, 0);
  }

}
model {
  for (t in 1 : 4)
    y[t] ~ normal(y_hat[t], sigma); // independent normal noise
}
