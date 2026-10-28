// TSVC "NodeSplitting" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` and `d` are locals).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. TSVC s1244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * c[i] + b[i]*b[i] + c[i]; d[i] = a[i] + a[i+1]; }
// Intent: cycle with true and anti dependency.
// Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
}
model {
  vector[N + 1] a5 = a0;
  vector[N] b5 = b0;
  vector[N] d5 = d0;
  for (n in 1 : N) {
    a5[n] = b5[n] + c[n] .* c[n];
    d5[n] = a5[n] + a5[n + 1];
  }
  target += sum(a5);
  target += sum(d5);
}
