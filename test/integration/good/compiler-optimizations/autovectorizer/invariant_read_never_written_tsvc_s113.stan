// Each iteration reads the fixed element `a2[1]`, which the loop never
// writes because `n` starts at 2. The pair of an affine and an invariant
// subscript is conservatively treated as confused, so the loop stays as
// written even though it is vectorizable.
// Source: TSVC s113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D; i++) a[i] = a[0] + b[i];
// Intent: vectorizable (a[1] is never written by the loop).
// Dependences: Affine vs Invariant subscript, confused
// Desired outcome: left as a loop.
// A weak-zero SIV test would prove the pair independent.
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
