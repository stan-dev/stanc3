// TSVC "NodeSplitting" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` is a local).
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 1. TSVC s2244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i+1] = b[i] + e[i]; a[i] = b[i] + c[i]; }
// Intent: vectorizable (the output dependence points forward).
// Design: output S1->S2 {Lt, 1}.
// Emitted: vec `a1[2:(N+1)] = b1 + e`, vec `a1[:] = b1 + c` (= GCC f5).
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
