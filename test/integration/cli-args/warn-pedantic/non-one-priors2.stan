data {
  real x;
  real y;
}
parameters {
  real a;
  real b;
  real c;
  real d;
  real e;
  real f;
}
model
{
  a ~ normal(0, 1);
  b ~ normal(a, 1);
  x ~ normal(b, 1);
  y ~ normal(c, 1);
  d ~ normal(b, 1);
  e ~ normal(a, 1);
  f ~ normal(a, 1);
  f ~ normal(e, 1);
}
