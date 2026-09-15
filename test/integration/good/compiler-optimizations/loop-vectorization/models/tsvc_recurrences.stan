// TSVC "Recurrences" loops, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). Every loop is a true recurrence: a self-edge with a
// non-Eq direction, or a two-statement cycle. None may be vectorized; each
// loop must appear unchanged in mir.expected.
data {
  int<lower=3> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N] a = a0;

  // TSVC s321 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] += a[i-1] * b[i];
  // intent: first order linear recurrence; not vectorizable
  // graph: true self `{Lt,1}`   emitted: seq
  for (n in 2 : N) {
    a[n] = a[n] + a[n - 1] * b[n];
  }

  // TSVC s322 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = a[i] + a[i-1] * b[i] + a[i-2] * c[i];
  // intent: second order linear recurrence; not vectorizable
  // graph: true self `{Lt,1}`, `{Lt,2}`   emitted: seq
  for (n in 3 : N) {
    a[n] = a[n] + a[n - 1] * b[n] + a[n - 2] * c[n];
  }

  // TSVC s323 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = b[i-1] + c[i] * d[i]; b[i] = a[i] + c[i] * e[i];
  // intent: coupled recurrence; not vectorizable
  // graph: `S1→S2 {Eq}`, `S2→S1 {Lt,1}`: cycle   emitted: seq
  {
    vector[N] b2 = b;
    for (n in 2 : N) {
      a[n] = b2[n - 1] + c[n] * d[n];
      b2[n] = a[n] + c[n] * e[n];
    }
    target += sum(b2);
  }

  target += sum(a);
}
