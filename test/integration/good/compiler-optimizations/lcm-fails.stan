data {
  int<lower=0> J; // number of schools
  array[J] real y; // estimated treatment effect (school j)
}
parameters {
  array[J] real theta;
}
model {
  y ~ normal(theta, 1);
}

