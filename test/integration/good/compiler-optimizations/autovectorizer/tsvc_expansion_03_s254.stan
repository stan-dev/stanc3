// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. TSVC s254 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = (b[i] + x) * 0.5; x = b[i];
// intent: carry-around variable
// graph: confused   emitted: seq
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
}
model {
  vector[N] a3 = a0;
  real x3 = 0;
  for (n in 1 : N) {
    a3[n] = (b[n] + x3) * 0.5;
    x3 = b[n];
  }
  target += sum(a3) + x3;
}
