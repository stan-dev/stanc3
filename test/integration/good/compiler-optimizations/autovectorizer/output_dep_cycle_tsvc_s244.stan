// TSVC "NodeSplitting" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` and `d` are locals).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. TSVC s244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; ++i) { a[i] = b[i] + c[i] * d[i]; b[i] = c[i] + b[i]; a[i+1] = b[i] + a[i+1] * d[i]; }
// Intent: false dependence cycle breaking.
// Design: output S3->S1 {Lt, 1}, S1->S2 {Eq}, S2->S3 {Eq}: cycle -> seq.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
}
model {
  vector[N + 1] a4 = a0;
  vector[N] b4 = b0;
  vector[N] d4 = d0;
  for (n in 1 : N) {
    a4[n] = b4[n] + c[n] .* d4[n];
    b4[n] = c[n] + b4[n];
    a4[n + 1] = b4[n] + a4[n + 1] .* d4[n];
  }
  target += sum(a4);
  target += sum(b4);
}
