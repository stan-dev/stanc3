// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 1. TSVC s251 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: s = b[i] + c[i] * d[i]; a[i] = s * s;
// intent: scalar expansion; vectorizable after expansion
// graph: `s1` confused (subs = [])   emitted: seq (roadmap item 2)
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a1 = a0;
  for (n in 1 : N) {
    real s1 = b[n] + c[n] * d[n];
    a1[n] = s1 * s1;
  }
  target += sum(a1);
}
