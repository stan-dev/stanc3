// TSVC "StatementReordering" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 1. TSVC s211 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D-1; i++) { a[i] = b[i - 1] + c[i] * d[i]; b[i] = b[i + 1] - e[i] * d[i]; }
// Intent: vectorizable after statement reordering.
// Design: S2 self anti-dependence: no edge; true S2->S1 {Lt, 1}.
// Emitted: vec S2 `b1[2:(N-1)] = b1[3:N] - e .* d`, then vec S1
// `a1[2:(N-1)] = b1[1:(N-2)] + c .* d` (reordered).
data {
  int<lower=3> N;
  vector[N + 1] a0;
  vector[N + 1] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N + 1] a1 = a0;
  vector[N + 1] b1 = b0;
  for (n in 2 : (N - 1)) {
    a1[n] = b1[n - 1] + c[n] .* d[n];
    b1[n] = b1[n + 1] - e[n] .* d[n];
  }
  target += sum(a1);
  target += sum(b1);
}
