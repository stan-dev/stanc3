// The second statement reads a[i + 1] before the first statement overwrites
// it in the next iteration. TSVC calls this a "dependency needing
// temporary". Moving the second statement first removes the need for the
// temporary. Translated to 1-based Stan, with written variables as
// model-block locals initialised from data.
// Source: TSVC s212 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] *= c[i];
//      b[i] += a[i + 1] * d[i]; }
// Dependences: anti S2->S1 {Lt, 1}, acyclic
// Desired outcome: fully vectorized.
//   `b2[:] = b2 + a2[2:(N+1)] .* d` then `a2[:] = a2 .* c`
data {
  int<lower=3> N;
  vector[N + 1] a0;
  vector[N + 1] b0;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N + 1] a2 = a0;
  vector[N + 1] b2 = b0;
  for (n in 1 : N) {
    a2[n] = a2[n] .* c[n];
    b2[n] = b2[n] + a2[n + 1] .* d[n];
  }
  target += sum(a2);
  target += sum(b2);
}
