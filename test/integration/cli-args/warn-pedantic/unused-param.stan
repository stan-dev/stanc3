parameters {
  real a;
  real b;
  real c;
  real d;
  real e;
}
transformed parameters {
  real f = c;
}
model {
  b ~ normal(1, 1);
  a ~ normal(b, 1);
}
generated quantities {
  real g = d;
}
