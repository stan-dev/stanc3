// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). An effectful statement (a void call) stays sequential
// but does not stop independent neighbours from being hoisted.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
// 1. TSVC s471 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: x[i] = b[i] + d[i] * d[i]; s471s(); b[i] = c[i] + d[i] * e[i];
// intent: call statement in the loop
// graph: S2 effectful; S1, S3 isolated
// emitted: S1 vec, S2 seq, S3 vec (S1 before S3 by position; `foo` cannot
//          touch `x1`/`b1` in Stan's value semantics)
functions {
  void foo() {
    print("foo");
  }
}
data {
  int<lower=1> N;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N] b1 = b0;
  vector[N] x1;
  for (n in 1 : N) {
    x1[n] = b1[n] + d[n] * d[n];
    foo();
    b1[n] = c[n] + d[n] * e[n];
  }
  target += sum(b1) + sum(x1);
}
