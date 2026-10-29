// Each iteration reads the fixed element `a3[N %/% 2]`, which the loop
// writes at n = N %/% 2, so later iterations read the new value. The pair is
// confused and the loop correctly stays as written.
// Source: TSVC s1113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D; i++) a[i] = a[LEN_1D/2] + b[i];
// Intent: one real dependence at n = N/2: not vectorizable as a whole.
// Dependences: confused
// Desired outcome: left as a loop.
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
