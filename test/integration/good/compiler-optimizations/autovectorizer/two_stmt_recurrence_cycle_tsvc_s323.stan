// S1 reads `b3[n - 1]` written by the previous iteration's S2, and S2 reads
// `a3[n]` written by S1. The two statements form a true recurrence cycle,
// so the loop may not be vectorized and stays as written.
// Source: TSVC s323 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = b[i-1] + c[i] * d[i]; b[i] = a[i] + c[i] * e[i];
// Intent: coupled recurrence, not vectorizable
// Dependences: `S1->S2 {Eq}`, `S2->S1 {Lt,1}`, a cycle
// Desired outcome: left as a loop.
data {
  int<lower=3> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N] a3 = a0;
  vector[N] b3 = b;
  for (n in 2 : N) {
    a3[n] = b3[n - 1] + c[n] * d[n];
    b3[n] = a3[n] + c[n] * e[n];
  }
  target += sum(a3) + sum(b3);
}
