// From the design examples of design-docs/active/vectorize-loop-fission.md
// section 7.8.1. Direction sets are for the loop over n; V marks a statement
// that becomes a vector statement, S one that stays in a sequential loop.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 1. Radon: every dependence is {Eq}. Three vector statements, no loop.
// edges: a->b {Eq}, b->c {Eq}. alpha, county_idx, log_uppm, ... are
// read-only: no edges.
// => muj1[:] = alpha[county_idx] + log_uppm * beta[1];
//    mu1[:]  = muj1 + floor_measure * beta[2];
//    target += normal_lpdf(log_radon | mu1, sigma_y);
data {
  int<lower=2> N;
  int<lower=1> J;
  array[N] int<lower=1, upper=J> county_idx;
  vector[N] log_uppm;
  vector[N] floor_measure;
  vector[N] log_radon;
}
parameters {
  vector[J] alpha;
  vector[2] beta;
  real<lower=0> sigma_y;
}
model {
  vector[N] mu1;
  vector[N] muj1;
  for (n in 1 : N) {
    muj1[n] = alpha[county_idx[n]] + log_uppm[n] * beta[1]; // V
    mu1[n] = muj1[n] + floor_measure[n] * beta[2]; // V
    target += normal_lpdf(log_radon[n] | mu1[n], sigma_y); // V
  }
}
