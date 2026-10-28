// TSVC "StatementReordering" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 2. TSVC s212 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] *= c[i]; b[i] += a[i + 1] * d[i]; }
// Intent: "dependency needing temporary".
// Design: anti S2->S1 {Lt, 1}, acyclic.
// Emitted: vec S2 `b2[:] = b2 + a2[2:(N+1)] .* d`, then vec S1
// `a2[:] = a2 .* c` (reordering replaces the temporary).
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
