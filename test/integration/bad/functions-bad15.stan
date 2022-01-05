functions {
  void unit_normal_lp(real u) {
    target += normal_lpdf(u|0,1);
  }
}
parameters {
  real y;
}
model {
  target += unit_normal_lp(y);
}
