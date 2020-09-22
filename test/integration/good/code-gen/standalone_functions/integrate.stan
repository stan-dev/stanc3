functions {

  vector integrand(vector x) {
    return exp(-square(x));
  }

  vector f(real t, vector z, real a, vector b) {
    return z;
  }

  real ode_integrate(vector vd, real rd, real[] rad) {
    vector[size(vd)] a[size(vd)] = ode_bdf_tol(f, vd, rd, rad, 1e-6, 1e-6, 100, rd, vd);
    return a[1][1];
  }

}
data {
}
model {}