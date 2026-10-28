// TSVC "LinearDependence" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 2. TSVC s113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D; i++) a[i] = a[0] + b[i];
// Intent: vectorizable (a[1] is never written by the loop).
// Design: Affine vs Invariant subscript: confused -> seq (conservative;
// weak-zero SIV test is roadmap item 3).
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b;
}
model {
  vector[N + 1] a2 = a0;
  for (n in 2 : N) {
    a2[n] = a2[1] + b[n];
  }
  target += sum(a2);
}
