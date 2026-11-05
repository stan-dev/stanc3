// A scatter to `a4[ip[n]]` through a read-only index array. The `Varying`
// subscript can only create a dependence when the indexed variable is also
// read in the loop. The loop does not read `a4`. The loop becomes one vector
// statement with a multi-index scatter.
// Source: TSVC s491 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[ip[i]] = b[i] + c[i] * d[i];
// Intent: indirect addressing, scatter, vectorizable
// Dependences: no edge
// Desired outcome: fully vectorized.
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a4 = a0;
  for (n in 1 : N) {
    a4[ip[n]] = b[n] + c[n] .* d[n];
  }
  target += sum(a4);
}
