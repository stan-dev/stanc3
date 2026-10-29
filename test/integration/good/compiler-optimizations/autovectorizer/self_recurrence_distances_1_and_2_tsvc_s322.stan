// `a2[n]` is updated from `a2[n - 1]` and `a2[n - 2]`, a second-order linear
// recurrence. The true self-dependences with non-Eq directions mean the loop
// may not be vectorized and stays as written.
// Source: TSVC s322 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = a[i] + a[i-1] * b[i] + a[i-2] * c[i];
// Intent: second order linear recurrence, not vectorizable
// Dependences: true self `{Lt,1}`, `{Lt,2}`
// Desired outcome: left as a loop.
data {
  int<lower=3> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
}
model {
  vector[N] a2 = a0;
  for (n in 3 : N) {
    a2[n] = a2[n] + a2[n - 1] * b[n] + a2[n - 2] * c[n];
  }
  target += sum(a2);
}
