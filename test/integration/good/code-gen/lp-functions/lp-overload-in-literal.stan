functions {
  real tlp_lp(real x) {
    return target() + x;
  }
  real tlp_lp(vector x) {
    return target() + sum(x);
  }
}
data {
  real d;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  array[2] real a = {tlp_lp(d), 1.0};
  target += a[1] + a[2];
}
