// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 6. TSVC s453 (Expansion / induction variable), UoB-HPC/TSVC_2 src/tsvc.c
// C: s += 2.; a[i] = s * b[i];
// intent: induction variable recognition
// graph: confused   emitted: seq
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
}
model {
  vector[N] a6 = a0;
  real s6 = 0;
  for (n in 1 : N) {
    s6 += 2;
    a6[n] = s6 * b[n];
  }
  target += sum(a6);
}
