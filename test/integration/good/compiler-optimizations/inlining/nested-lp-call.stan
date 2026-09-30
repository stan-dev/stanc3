functions {
  real twice(real x) {
    return x + x;
  }
  real tlp_lp(real x) {
    return target() + x;
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
  target += twice(tlp_lp(d) * 2);
  target += twice(target() * 2);
}
