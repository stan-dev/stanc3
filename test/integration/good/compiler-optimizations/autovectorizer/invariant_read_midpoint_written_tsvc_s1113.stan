// TSVC "LinearDependence" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. TSVC s1113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D; i++) a[i] = a[LEN_1D/2] + b[i];
// Intent: one real dependence at n = N/2: not vectorizable as a whole.
// Design: confused -> seq (correct).
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b;
}
model {
  vector[N + 1] a3 = a0;
  for (n in 1 : N) {
    a3[n] = a3[N %/% 2] + b[n];
  }
  target += sum(a3);
}
