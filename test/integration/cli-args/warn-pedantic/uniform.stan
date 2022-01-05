parameters {
  real a;
  real<lower=0, upper=1> b;
  real<lower=0> c;
  real<upper=0> d;
}
model {
  a ~ uniform(0, 1);
  1 ~ uniform(0, a);
  b ~ uniform(0, 1);
  c ~ uniform(0, 1);
  d ~ uniform(0, 1);
}
