// `a1[n]` is updated from `a1[n - 1]`, a first-order linear recurrence. The
// true self-dependence with a non-Eq direction means the loop may not be
// vectorized and stays as written.
// Source: TSVC s321 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += a[i-1] * b[i];
// Intent: first order linear recurrence, not vectorizable
// Dependences: true self `{Lt,1}`
// Desired outcome: left as a loop.
data {
  int<lower=3> N;
  vector[N] a0;
  vector[N] b;
}
model {
  vector[N] a1 = a0;
  for (n in 2 : N) {
    a1[n] = a1[n] + a1[n - 1] * b[n];
  }
  target += sum(a1);
}
