// TSVC "LinearDependence" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. TSVC s293 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D; i++) a[i] = a[0];
// Intent: actual dependence cycle through a[0].
// Design: confused -> seq (correct; contrast s113).
data {
  int<lower=2> N;
  vector[N + 1] a0;
}
model {
  vector[N + 1] a4 = a0;
  for (n in 1 : N) {
    a4[n] = a4[1];
  }
  target += sum(a4);
}
