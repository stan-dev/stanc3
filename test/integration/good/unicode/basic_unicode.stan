data {
  int<lower=0> J;             // number of schools
  array[J] real y;                  // estimated treatment effect (school j)
  array[J] real<lower=0> σ;         // std err of effect estimate (school j)
}
parameters {
  real μ;
  array[J] real θ;
  real<lower=0> τ;
}
model {
  θ ~ normal(μ, τ);
  y ~ normal(θ, σ);
}
