// TSVC "LinearDependence" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 1. TSVC s112 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = LEN_1D - 2; i >= 0; i--) a[i+1] = a[i] + b[i];
// Intent: vectorizable only by loop reversal.
// Design: true self-dependence {Lt, distance 1}: recurrence -> seq
// (loop unchanged).
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b;
}
model {
  vector[N + 1] a1 = a0;
  for (n in 1 : N) {
    a1[n + 1] = a1[n] + b[n];
  }
  target += sum(a1);
}
