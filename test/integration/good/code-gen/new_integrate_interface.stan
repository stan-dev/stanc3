functions {
  vector f(real t, vector z, real a, vector b) {
    return z;
  }
}
data {
  int N;
  int id;
  real rd;
  array[N] real rad;
  vector[N] vd;
}
transformed data {
  array[N] vector[N] zd = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd,
                                      vd);
  zd = ode_bdf_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zd = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zd = ode_adams_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zd = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zd = ode_rk45_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zd = ode_bdf(f, vd, rd, rad, rd, vd);
  zd = ode_bdf(f, vd, id, rad, rd, vd);
  zd = ode_adams(f, vd, rd, rad, rd, vd);
  zd = ode_adams(f, vd, id, rad, rd, vd);
  zd = ode_rk45(f, vd, rd, rad, rd, vd);
  zd = ode_rk45(f, vd, id, rad, rd, vd);
}
parameters {
  real r;
  array[N] real ra;
  vector[N] v;
}
transformed parameters {
  array[N] vector[N] z = ode_bdf_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  
  z = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  z = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  z = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  z = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  z = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  z = ode_adams(f, vd, id, rad, rd, vd);
  z = ode_adams(f, vd, rd, rad, rd, vd);
  z = ode_adams(f, vd, rd, rad, rd, v);
  z = ode_adams(f, vd, rd, rad, r, vd);
  z = ode_adams(f, vd, rd, rad, r, v);
  z = ode_adams(f, vd, rd, ra, rd, vd);
  z = ode_adams(f, vd, rd, ra, rd, v);
  z = ode_adams(f, vd, rd, ra, r, vd);
  z = ode_adams(f, vd, rd, ra, r, v);
  z = ode_adams(f, vd, r, rad, rd, vd);
  z = ode_adams(f, vd, r, rad, rd, v);
  z = ode_adams(f, vd, r, rad, r, vd);
  z = ode_adams(f, vd, r, rad, r, v);
  z = ode_adams(f, vd, r, ra, rd, vd);
  z = ode_adams(f, vd, r, ra, rd, v);
  z = ode_adams(f, vd, r, ra, r, vd);
  z = ode_adams(f, vd, r, ra, r, v);
  z = ode_adams(f, v, rd, rad, rd, vd);
  z = ode_adams(f, v, rd, rad, rd, v);
  z = ode_adams(f, v, rd, rad, r, vd);
  z = ode_adams(f, v, rd, rad, r, v);
  z = ode_adams(f, v, rd, ra, rd, vd);
  z = ode_adams(f, v, rd, ra, rd, v);
  z = ode_adams(f, v, rd, ra, r, vd);
  z = ode_adams(f, v, rd, ra, r, v);
  z = ode_adams(f, v, r, rad, rd, vd);
  z = ode_adams(f, v, r, rad, rd, v);
  z = ode_adams(f, v, r, rad, r, vd);
  z = ode_adams(f, v, r, rad, r, v);
  z = ode_adams(f, v, r, ra, rd, vd);
  z = ode_adams(f, v, r, ra, rd, v);
  z = ode_adams(f, v, r, ra, r, vd);
  z = ode_adams(f, v, r, ra, r, v);
  z = ode_bdf(f, vd, id, rad, rd, vd);
  z = ode_bdf(f, vd, rd, rad, rd, vd);
  z = ode_bdf(f, vd, rd, rad, rd, v);
  z = ode_bdf(f, vd, rd, rad, r, vd);
  z = ode_bdf(f, vd, rd, rad, r, v);
  z = ode_bdf(f, vd, rd, ra, rd, vd);
  z = ode_bdf(f, vd, rd, ra, rd, v);
  z = ode_bdf(f, vd, rd, ra, r, vd);
  z = ode_bdf(f, vd, rd, ra, r, v);
  z = ode_bdf(f, vd, r, rad, rd, vd);
  z = ode_bdf(f, vd, r, rad, rd, v);
  z = ode_bdf(f, vd, r, rad, r, vd);
  z = ode_bdf(f, vd, r, rad, r, v);
  z = ode_bdf(f, vd, r, ra, rd, vd);
  z = ode_bdf(f, vd, r, ra, rd, v);
  z = ode_bdf(f, vd, r, ra, r, vd);
  z = ode_bdf(f, vd, r, ra, r, v);
  z = ode_bdf(f, v, rd, rad, rd, vd);
  z = ode_bdf(f, v, rd, rad, rd, v);
  z = ode_bdf(f, v, rd, rad, r, vd);
  z = ode_bdf(f, v, rd, rad, r, v);
  z = ode_bdf(f, v, rd, ra, rd, vd);
  z = ode_bdf(f, v, rd, ra, rd, v);
  z = ode_bdf(f, v, rd, ra, r, vd);
  z = ode_bdf(f, v, rd, ra, r, v);
  z = ode_bdf(f, v, r, rad, rd, vd);
  z = ode_bdf(f, v, r, rad, rd, v);
  z = ode_bdf(f, v, r, rad, r, vd);
  z = ode_bdf(f, v, r, rad, r, v);
  z = ode_bdf(f, v, r, ra, rd, vd);
  z = ode_bdf(f, v, r, ra, rd, v);
  z = ode_bdf(f, v, r, ra, r, vd);
  z = ode_bdf(f, v, r, ra, r, v);
  z = ode_rk45(f, vd, id, rad, rd, vd);
  z = ode_rk45(f, vd, rd, rad, rd, vd);
  z = ode_rk45(f, vd, rd, rad, rd, v);
  z = ode_rk45(f, vd, rd, rad, r, vd);
  z = ode_rk45(f, vd, rd, rad, r, v);
  z = ode_rk45(f, vd, rd, ra, rd, vd);
  z = ode_rk45(f, vd, rd, ra, rd, v);
  z = ode_rk45(f, vd, rd, ra, r, vd);
  z = ode_rk45(f, vd, rd, ra, r, v);
  z = ode_rk45(f, vd, r, rad, rd, vd);
  z = ode_rk45(f, vd, r, rad, rd, v);
  z = ode_rk45(f, vd, r, rad, r, vd);
  z = ode_rk45(f, vd, r, rad, r, v);
  z = ode_rk45(f, vd, r, ra, rd, vd);
  z = ode_rk45(f, vd, r, ra, rd, v);
  z = ode_rk45(f, vd, r, ra, r, vd);
  z = ode_rk45(f, vd, r, ra, r, v);
  z = ode_rk45(f, v, rd, rad, rd, vd);
  z = ode_rk45(f, v, rd, rad, rd, v);
  z = ode_rk45(f, v, rd, rad, r, vd);
  z = ode_rk45(f, v, rd, rad, r, v);
  z = ode_rk45(f, v, rd, ra, rd, vd);
  z = ode_rk45(f, v, rd, ra, rd, v);
  z = ode_rk45(f, v, rd, ra, r, vd);
  z = ode_rk45(f, v, rd, ra, r, v);
  z = ode_rk45(f, v, r, rad, rd, vd);
  z = ode_rk45(f, v, r, rad, rd, v);
  z = ode_rk45(f, v, r, rad, r, vd);
  z = ode_rk45(f, v, r, rad, r, v);
  z = ode_rk45(f, v, r, ra, rd, vd);
  z = ode_rk45(f, v, r, ra, rd, v);
  z = ode_rk45(f, v, r, ra, r, vd);
  z = ode_rk45(f, v, r, ra, r, v);
}
model {
  array[N] vector[N] zm = ode_bdf_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd,
                                      vd);
  
  zm = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zm = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zm = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zm = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zm = ode_adams(f, vd, id, rad, rd, vd);
  zm = ode_adams(f, vd, rd, rad, rd, vd);
  zm = ode_adams(f, vd, rd, rad, rd, v);
  zm = ode_adams(f, vd, rd, rad, r, vd);
  zm = ode_adams(f, vd, rd, rad, r, v);
  zm = ode_adams(f, vd, rd, ra, rd, vd);
  zm = ode_adams(f, vd, rd, ra, rd, v);
  zm = ode_adams(f, vd, rd, ra, r, vd);
  zm = ode_adams(f, vd, rd, ra, r, v);
  zm = ode_adams(f, vd, r, rad, rd, vd);
  zm = ode_adams(f, vd, r, rad, rd, v);
  zm = ode_adams(f, vd, r, rad, r, vd);
  zm = ode_adams(f, vd, r, rad, r, v);
  zm = ode_adams(f, vd, r, ra, rd, vd);
  zm = ode_adams(f, vd, r, ra, rd, v);
  zm = ode_adams(f, vd, r, ra, r, vd);
  zm = ode_adams(f, vd, r, ra, r, v);
  zm = ode_adams(f, v, rd, rad, rd, vd);
  zm = ode_adams(f, v, rd, rad, rd, v);
  zm = ode_adams(f, v, rd, rad, r, vd);
  zm = ode_adams(f, v, rd, rad, r, v);
  zm = ode_adams(f, v, rd, ra, rd, vd);
  zm = ode_adams(f, v, rd, ra, rd, v);
  zm = ode_adams(f, v, rd, ra, r, vd);
  zm = ode_adams(f, v, rd, ra, r, v);
  zm = ode_adams(f, v, r, rad, rd, vd);
  zm = ode_adams(f, v, r, rad, rd, v);
  zm = ode_adams(f, v, r, rad, r, vd);
  zm = ode_adams(f, v, r, rad, r, v);
  zm = ode_adams(f, v, r, ra, rd, vd);
  zm = ode_adams(f, v, r, ra, rd, v);
  zm = ode_adams(f, v, r, ra, r, vd);
  zm = ode_adams(f, v, r, ra, r, v);
  zm = ode_rk45(f, vd, id, rad, rd, vd);
  zm = ode_rk45(f, vd, rd, rad, rd, vd);
  zm = ode_rk45(f, vd, rd, rad, rd, v);
  zm = ode_rk45(f, vd, rd, rad, r, vd);
  zm = ode_rk45(f, vd, rd, rad, r, v);
  zm = ode_rk45(f, vd, rd, ra, rd, vd);
  zm = ode_rk45(f, vd, rd, ra, rd, v);
  zm = ode_rk45(f, vd, rd, ra, r, vd);
  zm = ode_rk45(f, vd, rd, ra, r, v);
  zm = ode_rk45(f, vd, r, rad, rd, vd);
  zm = ode_rk45(f, vd, r, rad, rd, v);
  zm = ode_rk45(f, vd, r, rad, r, vd);
  zm = ode_rk45(f, vd, r, rad, r, v);
  zm = ode_rk45(f, vd, r, ra, rd, vd);
  zm = ode_rk45(f, vd, r, ra, rd, v);
  zm = ode_rk45(f, vd, r, ra, r, vd);
  zm = ode_rk45(f, vd, r, ra, r, v);
  zm = ode_rk45(f, v, rd, rad, rd, vd);
  zm = ode_rk45(f, v, rd, rad, rd, v);
  zm = ode_rk45(f, v, rd, rad, r, vd);
  zm = ode_rk45(f, v, rd, rad, r, v);
  zm = ode_rk45(f, v, rd, ra, rd, vd);
  zm = ode_rk45(f, v, rd, ra, rd, v);
  zm = ode_rk45(f, v, rd, ra, r, vd);
  zm = ode_rk45(f, v, rd, ra, r, v);
  zm = ode_rk45(f, v, r, rad, rd, vd);
  zm = ode_rk45(f, v, r, rad, rd, v);
  zm = ode_rk45(f, v, r, rad, r, vd);
  zm = ode_rk45(f, v, r, rad, r, v);
  zm = ode_rk45(f, v, r, ra, rd, vd);
  zm = ode_rk45(f, v, r, ra, rd, v);
  zm = ode_rk45(f, v, r, ra, r, vd);
  zm = ode_rk45(f, v, r, ra, r, v);
  zm = ode_bdf(f, vd, id, rad, rd, vd);
  zm = ode_bdf(f, vd, rd, rad, rd, vd);
  zm = ode_bdf(f, vd, rd, rad, rd, v);
  zm = ode_bdf(f, vd, rd, rad, r, vd);
  zm = ode_bdf(f, vd, rd, rad, r, v);
  zm = ode_bdf(f, vd, rd, ra, rd, vd);
  zm = ode_bdf(f, vd, rd, ra, rd, v);
  zm = ode_bdf(f, vd, rd, ra, r, vd);
  zm = ode_bdf(f, vd, rd, ra, r, v);
  zm = ode_bdf(f, vd, r, rad, rd, vd);
  zm = ode_bdf(f, vd, r, rad, rd, v);
  zm = ode_bdf(f, vd, r, rad, r, vd);
  zm = ode_bdf(f, vd, r, rad, r, v);
  zm = ode_bdf(f, vd, r, ra, rd, vd);
  zm = ode_bdf(f, vd, r, ra, rd, v);
  zm = ode_bdf(f, vd, r, ra, r, vd);
  zm = ode_bdf(f, vd, r, ra, r, v);
  zm = ode_bdf(f, v, rd, rad, rd, vd);
  zm = ode_bdf(f, v, rd, rad, rd, v);
  zm = ode_bdf(f, v, rd, rad, r, vd);
  zm = ode_bdf(f, v, rd, rad, r, v);
  zm = ode_bdf(f, v, rd, ra, rd, vd);
  zm = ode_bdf(f, v, rd, ra, rd, v);
  zm = ode_bdf(f, v, rd, ra, r, vd);
  zm = ode_bdf(f, v, rd, ra, r, v);
  zm = ode_bdf(f, v, r, rad, rd, vd);
  zm = ode_bdf(f, v, r, rad, rd, v);
  zm = ode_bdf(f, v, r, rad, r, vd);
  zm = ode_bdf(f, v, r, rad, r, v);
  zm = ode_bdf(f, v, r, ra, rd, vd);
  zm = ode_bdf(f, v, r, ra, rd, v);
  zm = ode_bdf(f, v, r, ra, r, vd);
  zm = ode_bdf(f, v, r, ra, r, v);
  r ~ normal(0, 1);
}
generated quantities {
  array[N] vector[N] zg = ode_bdf_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd,
                                      vd);
  zg = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_bdf_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_adams_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, vd, id, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, vd, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, vd, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, vd, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, v, rd, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, v, rd, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, v, r, rad, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, vd);
  zg = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, rd, v);
  zg = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, vd);
  zg = ode_rk45_tol(f, v, r, ra, 1e-6, 1e-6, 100, r, v);
  zg = ode_rk45(f, vd, id, rad, rd, vd);
  zg = ode_rk45(f, vd, rd, rad, rd, vd);
  zg = ode_rk45(f, vd, rd, rad, rd, v);
  zg = ode_rk45(f, vd, rd, rad, r, vd);
  zg = ode_rk45(f, vd, rd, rad, r, v);
  zg = ode_rk45(f, vd, rd, ra, rd, vd);
  zg = ode_rk45(f, vd, rd, ra, rd, v);
  zg = ode_rk45(f, vd, rd, ra, r, vd);
  zg = ode_rk45(f, vd, rd, ra, r, v);
  zg = ode_rk45(f, vd, r, rad, rd, vd);
  zg = ode_rk45(f, vd, r, rad, rd, v);
  zg = ode_rk45(f, vd, r, rad, r, vd);
  zg = ode_rk45(f, vd, r, rad, r, v);
  zg = ode_rk45(f, vd, r, ra, rd, vd);
  zg = ode_rk45(f, vd, r, ra, rd, v);
  zg = ode_rk45(f, vd, r, ra, r, vd);
  zg = ode_rk45(f, vd, r, ra, r, v);
  zg = ode_rk45(f, v, rd, rad, rd, vd);
  zg = ode_rk45(f, v, rd, rad, rd, v);
  zg = ode_rk45(f, v, rd, rad, r, vd);
  zg = ode_rk45(f, v, rd, rad, r, v);
  zg = ode_rk45(f, v, rd, ra, rd, vd);
  zg = ode_rk45(f, v, rd, ra, rd, v);
  zg = ode_rk45(f, v, rd, ra, r, vd);
  zg = ode_rk45(f, v, rd, ra, r, v);
  zg = ode_rk45(f, v, r, rad, rd, vd);
  zg = ode_rk45(f, v, r, rad, rd, v);
  zg = ode_rk45(f, v, r, rad, r, vd);
  zg = ode_rk45(f, v, r, rad, r, v);
  zg = ode_rk45(f, v, r, ra, rd, vd);
  zg = ode_rk45(f, v, r, ra, rd, v);
  zg = ode_rk45(f, v, r, ra, r, vd);
  zg = ode_rk45(f, v, r, ra, r, v);
  zg = ode_adams(f, vd, id, rad, rd, vd);
  zg = ode_adams(f, vd, rd, rad, rd, vd);
  zg = ode_adams(f, vd, rd, rad, rd, v);
  zg = ode_adams(f, vd, rd, rad, r, vd);
  zg = ode_adams(f, vd, rd, rad, r, v);
  zg = ode_adams(f, vd, rd, ra, rd, vd);
  zg = ode_adams(f, vd, rd, ra, rd, v);
  zg = ode_adams(f, vd, rd, ra, r, vd);
  zg = ode_adams(f, vd, rd, ra, r, v);
  zg = ode_adams(f, vd, r, rad, rd, vd);
  zg = ode_adams(f, vd, r, rad, rd, v);
  zg = ode_adams(f, vd, r, rad, r, vd);
  zg = ode_adams(f, vd, r, rad, r, v);
  zg = ode_adams(f, vd, r, ra, rd, vd);
  zg = ode_adams(f, vd, r, ra, rd, v);
  zg = ode_adams(f, vd, r, ra, r, vd);
  zg = ode_adams(f, vd, r, ra, r, v);
  zg = ode_adams(f, v, rd, rad, rd, vd);
  zg = ode_adams(f, v, rd, rad, rd, v);
  zg = ode_adams(f, v, rd, rad, r, vd);
  zg = ode_adams(f, v, rd, rad, r, v);
  zg = ode_adams(f, v, rd, ra, rd, vd);
  zg = ode_adams(f, v, rd, ra, rd, v);
  zg = ode_adams(f, v, rd, ra, r, vd);
  zg = ode_adams(f, v, rd, ra, r, v);
  zg = ode_adams(f, v, r, rad, rd, vd);
  zg = ode_adams(f, v, r, rad, rd, v);
  zg = ode_adams(f, v, r, rad, r, vd);
  zg = ode_adams(f, v, r, rad, r, v);
  zg = ode_adams(f, v, r, ra, rd, vd);
  zg = ode_adams(f, v, r, ra, rd, v);
  zg = ode_adams(f, v, r, ra, r, vd);
  zg = ode_adams(f, v, r, ra, r, v);
  zg = ode_bdf(f, vd, id, rad, rd, vd);
  zg = ode_bdf(f, vd, rd, rad, rd, vd);
  zg = ode_bdf(f, vd, rd, rad, rd, v);
  zg = ode_bdf(f, vd, rd, rad, r, vd);
  zg = ode_bdf(f, vd, rd, rad, r, v);
  zg = ode_bdf(f, vd, rd, ra, rd, vd);
  zg = ode_bdf(f, vd, rd, ra, rd, v);
  zg = ode_bdf(f, vd, rd, ra, r, vd);
  zg = ode_bdf(f, vd, rd, ra, r, v);
  zg = ode_bdf(f, vd, r, rad, rd, vd);
  zg = ode_bdf(f, vd, r, rad, rd, v);
  zg = ode_bdf(f, vd, r, rad, r, vd);
  zg = ode_bdf(f, vd, r, rad, r, v);
  zg = ode_bdf(f, vd, r, ra, rd, vd);
  zg = ode_bdf(f, vd, r, ra, rd, v);
  zg = ode_bdf(f, vd, r, ra, r, vd);
  zg = ode_bdf(f, vd, r, ra, r, v);
  zg = ode_bdf(f, v, rd, rad, rd, vd);
  zg = ode_bdf(f, v, rd, rad, rd, v);
  zg = ode_bdf(f, v, rd, rad, r, vd);
  zg = ode_bdf(f, v, rd, rad, r, v);
  zg = ode_bdf(f, v, rd, ra, rd, vd);
  zg = ode_bdf(f, v, rd, ra, rd, v);
  zg = ode_bdf(f, v, rd, ra, r, vd);
  zg = ode_bdf(f, v, rd, ra, r, v);
  zg = ode_bdf(f, v, r, rad, rd, vd);
  zg = ode_bdf(f, v, r, rad, rd, v);
  zg = ode_bdf(f, v, r, rad, r, vd);
  zg = ode_bdf(f, v, r, rad, r, v);
  zg = ode_bdf(f, v, r, ra, rd, vd);
  zg = ode_bdf(f, v, r, ra, rd, v);
  zg = ode_bdf(f, v, r, ra, r, vd);
  zg = ode_bdf(f, v, r, ra, r, v);
}

