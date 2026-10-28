// TSVC "NodeSplitting" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` and `d` are locals).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 2. TSVC s241 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] * c[i] * d[i]; b[i] = a[i] * a[i+1] * d[i]; }
// Intent: needs preloading of a[i+1] (node splitting).
// Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq (correct
// without node splitting).
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
}
model {
  vector[N + 1] a2 = a0;
  vector[N] b2 = b0;
  vector[N] d2 = d0;
  for (n in 1 : N) {
    a2[n] = b2[n] .* c[n] .* d2[n];
    b2[n] = a2[n] .* a2[n + 1] .* d2[n];
  }
  target += sum(a2);
  target += sum(b2);
}
