functions {
  real tlp_lp(real x) {
    return target() + x;
  }
  vector vlp_lp(vector x) {
    return x * target();
  }
}
data {
  real d;
  vector[3] dv;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  target += tlp_lp(d);
  target += sum(vlp_lp(dv));
}
