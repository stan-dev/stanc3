// `a1[n + 1]` is computed from `a1[n]` in a forward loop. Each iteration
// reads the value the previous iteration wrote. Because the true
// self-dependence with distance 1 is a recurrence, the loop stays as written.
// Source: TSVC s112 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = LEN_1D - 2; i >= 0; i--) a[i+1] = a[i] + b[i];
// Intent: vectorizable only by loop reversal.
// Dependences: true self-dependence {Lt, distance 1}, a recurrence
// Desired outcome: left as a loop.
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
