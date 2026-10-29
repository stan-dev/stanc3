// A sparse dot product written as a density, with a gather `b[ip[n]]`
// through a read-only index array. `b` is never written in the loop and the
// `target +=` increments are a reduction, so there is no dependence and the
// loop becomes one vector statement with a multi-index gather.
// Source: TSVC s4115 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: sum += a[i] * b[ip[i]];   (sparse dot product, written as a density)
// Intent: indirect addressing, reduction
// Dependences: reduction, no edge
// Desired outcome: fully vectorized.
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] b;
  vector[N] a_data;
}
parameters {
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(a_data[n] | b[ip[n]], sigma);
  }
}
