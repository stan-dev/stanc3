// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. TSVC s452 (Control Flow / induction), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = b[i] + c[i] * (real_t) (i+1);
// intent: induction variable used as a value
// graph: no edge   emitted: seq (existing bail "loop variable used as a value")
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
}
model {
  vector[N] a5 = a0;
  for (n in 1 : N) {
    a5[n] = b0[n] + c[n] * (n + 1);
  }
  target += sum(a5);
}
