/* $ stanc --auto-format ../funs3.stan : [exit 0] */
functions {
  real unit_normal_lpdf(real y) {
    return normal_lpdf(y | 0, 1);
  }
}
parameters {
  real y;
}
model {
  y ~ unit_normal();
}

