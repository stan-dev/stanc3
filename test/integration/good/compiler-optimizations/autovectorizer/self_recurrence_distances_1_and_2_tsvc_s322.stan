// TSVC "Recurrences" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop is a true recurrence: a self-edge with a
// non-Eq direction. The loop may not be vectorized and must appear
// unchanged in mir.expected.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 2. TSVC s322 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = a[i] + a[i-1] * b[i] + a[i-2] * c[i];
// intent: second order linear recurrence; not vectorizable
// graph: true self `{Lt,1}`, `{Lt,2}`   emitted: seq
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
