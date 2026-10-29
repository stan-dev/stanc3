// A scatter to `a2[ip[n]]` and a gather from `b[ip[n]]` through a read-only
// index array. `a2` is written but never read in the loop and `b` is never
// written, so there is no dependence and the loop becomes one vector
// statement with a multi-index gather and scatter.
// Source: TSVC s4113 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[ip[i]] = b[ip[i]] + c[i];
// Intent: indirect addressing, scatter and gather, vectorizable
// Dependences: `a2` written only, never read, so no edge
// Desired outcome: fully vectorized, as `a2[ip] = b[ip] + c`.
// Stan multi-index assignment is in-order, so duplicate indices behave as in
// the loop.
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
}
model {
  vector[N] a2 = a0;
  for (n in 1 : N) {
    a2[ip[n]] = b[ip[n]] + c[n];
  }
  target += sum(a2);
}
