// TSVC "Recurrences" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop is a true recurrence: a two-statement
// cycle. The loop may not be vectorized and must appear unchanged in
// mir.expected.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. TSVC s323 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = b[i-1] + c[i] * d[i]; b[i] = a[i] + c[i] * e[i];
// intent: coupled recurrence; not vectorizable
// graph: `S1->S2 {Eq}`, `S2->S1 {Lt,1}`: cycle   emitted: seq
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
