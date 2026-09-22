// TSVC "Recurrences" loops, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). Every loop is a true recurrence: a self-edge with a
// non-Eq direction, or a two-statement cycle. None may be vectorized; each
// loop must appear unchanged in mir.expected.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example. Every loop here is left unchanged
// by the pass.
data {
  int<lower=3> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  // ---- Left unchanged: no statement can be hoisted ----

  // 1. TSVC s321 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] += a[i-1] * b[i];
  // intent: first order linear recurrence; not vectorizable
  // graph: true self `{Lt,1}`   emitted: seq
  {
    vector[N] a1 = a0;
    for (n in 2 : N) {
      a1[n] = a1[n] + a1[n - 1] * b[n];
    }
    target += sum(a1);
  }

  // 2. TSVC s322 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = a[i] + a[i-1] * b[i] + a[i-2] * c[i];
  // intent: second order linear recurrence; not vectorizable
  // graph: true self `{Lt,1}`, `{Lt,2}`   emitted: seq
  {
    vector[N] a2 = a0;
    for (n in 3 : N) {
      a2[n] = a2[n] + a2[n - 1] * b[n] + a2[n - 2] * c[n];
    }
    target += sum(a2);
  }

  // 3. TSVC s323 (Recurrences), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = b[i-1] + c[i] * d[i]; b[i] = a[i] + c[i] * e[i];
  // intent: coupled recurrence; not vectorizable
  // graph: `S1→S2 {Eq}`, `S2→S1 {Lt,1}`: cycle   emitted: seq
  {
    vector[N] a3 = a0;
    vector[N] b3 = b;
    for (n in 2 : N) {
      a3[n] = b3[n - 1] + c[n] * d[n];
      b3[n] = a3[n] + c[n] * e[n];
    }
    target += sum(a3) + sum(b3);
  }
}
