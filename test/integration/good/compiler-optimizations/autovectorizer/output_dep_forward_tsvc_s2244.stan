// Two writes to `a1` in each iteration, `a1[n + 1]` and then `a1[n]`. Because
// the output dependence points forward, both statements become vector
// statements in their original order.
// Source: TSVC s2244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i+1] = b[i] + e[i]; a[i] = b[i] + c[i]; }
// Intent: vectorizable (the output dependence points forward).
// Dependences: output S1->S2 {Lt, 1}
// Desired outcome: fully vectorized, as `a1[2:(N+1)] = b1 + e` then
//   `a1[1:N] = b1 + c`.
// Same loop as output_dep_forward_gcc_vect_f5.stan.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] c;
  vector[N] e;
}
model {
  vector[N + 1] a1 = a0;
  vector[N] b1 = b0;
  for (n in 1 : N) {
    a1[n + 1] = b1[n] + e[n];
    a1[n] = b1[n] + c[n];
  }
  target += sum(a1);
}
