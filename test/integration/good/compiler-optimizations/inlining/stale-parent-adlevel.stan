functions {
  real g(real x) {
    return x * 3;
  }
  real tlp_lp(real x) {
    return target() + x;
  }
  complex cz(complex z) {
    return z * 2;
  }
  real tf(tuple(real, int) t) {
    return t.1 * 10 + t.2;
  }
}
data {
  real d;
  int c;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  real y1 = g(d) * 2;
  target += y1 * theta;
  real y2 = tlp_lp(d) * 2;
  target += y2;
  target += get_real(cz(g(d))) * theta;
  real y3 = (c ? g(d) : d) * 2;
  target += y3 * theta;
  target += tf((g(d), 1)) * theta;
}
