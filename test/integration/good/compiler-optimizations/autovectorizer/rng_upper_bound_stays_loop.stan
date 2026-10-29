// The loop runs to `poisson_rng(10)` and assigns `x[i] = exp(a[i])`. A vector
// statement would need the upper bound in two places, the loop range and the
// slice, and each copy would draw a different number.
// Desired outcome: left as a loop.
// Loop bounds that cannot be duplicated, such as rng calls, keep the loop.
data {
  int<lower=0> N;
  vector[N] a;
}
transformed data {
  vector[N] x;
  for (i in 1 : poisson_rng(10)) {
    x[i] = exp(a[i]);
  }
}
