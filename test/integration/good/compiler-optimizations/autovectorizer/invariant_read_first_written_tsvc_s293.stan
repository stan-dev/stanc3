// Each iteration copies the fixed element `a4[1]`, which the first iteration
// writes. The pair is confused and the loop correctly stays as written.
// Source: TSVC s293 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D; i++) a[i] = a[0];
// Intent: actual dependence cycle through a[0].
// Dependences: confused
// Desired outcome: left as a loop.
// Contrast invariant_read_never_written_tsvc_s113.stan, where the loop never
// writes the fixed element.
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
