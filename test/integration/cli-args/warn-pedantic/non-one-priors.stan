data {
  real x;
}
parameters {
  real a;
  real b;
  real c;
  real d;
}
model
{
  a ~ normal(0, 1);
  b ~ normal(0, 1);
  c ~ normal(a, b);
  d ~ normal(0, 1);
  x ~ normal(c, d);
}
