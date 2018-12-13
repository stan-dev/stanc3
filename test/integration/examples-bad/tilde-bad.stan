data {
  int<lower=0> N;
}
parameters {
  vector[N] foo;
}
model {
  foo ~ normal_lpdf(0, 1);
}