// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. TSVC s261 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: t = a[i] + b[i]; a[i] = t + c[i-1]; t = c[i] * d[i]; c[i] = t;
// intent: scalar renaming
// graph: confused   emitted: seq
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a5 = a0;
  vector[N] c5 = c;
  real t5 = 0;
  for (n in 2 : N) {
    t5 = a5[n] + b[n];
    a5[n] = t5 + c5[n - 1];
    t5 = c5[n] * d[n];
    c5[n] = t5;
  }
  target += sum(a5) + sum(c5);
}
