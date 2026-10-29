// A gather `b[ip[n]]` through a read-only index array. The `Varying`
// subscript can only create a dependence when the gathered variable is also
// written in the loop. `b` never is, so the loop becomes one vector
// statement with a multi-index gather.
// Source: TSVC s4112 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += b[ip[i]] * s;
// Intent: indirect addressing, gather, vectorizable
// Dependences: `b` read-only, no edge
// Desired outcome: fully vectorized.
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  real s;
}
model {
  vector[N] a1 = a0;
  for (n in 1 : N) {
    a1[n] = a1[n] + b[ip[n]] * s;
  }
  target += sum(a1);
}
