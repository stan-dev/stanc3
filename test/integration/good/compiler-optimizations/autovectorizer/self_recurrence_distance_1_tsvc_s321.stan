// TSVC "Recurrences" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop is a true recurrence: a self-edge with a
// non-Eq direction. The loop may not be vectorized and must appear
// unchanged in mir.expected.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 1. TSVC s321 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += a[i-1] * b[i];
// intent: first order linear recurrence; not vectorizable
// graph: true self `{Lt,1}`   emitted: seq
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
