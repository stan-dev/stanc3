// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 2. TSVC s252 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: s = b[i] * c[i]; a[i] = s + t; t = s;
// intent: loop with ambiguous scalar temporary
// graph: confused   emitted: seq
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
}
model {
  vector[N] a2 = a0;
  real t2 = 0;
  for (n in 1 : N) {
    real s2 = b[n] * c[n];
    a2[n] = s2 + t2;
    t2 = s2;
  }
  target += sum(a2) + t2;
}
