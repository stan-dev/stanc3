// A scalar `t5` is reused for two different temporaries in each iteration,
// and `a5[n]` reads `c5[n - 1]` from the previous iteration. The
// whole-variable access to `t5` (subs = []) is confused, so the loop stays
// as written until scalar expansion is implemented.
// Source: TSVC s261 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: t = a[i] + b[i]; a[i] = t + c[i-1]; t = c[i] * d[i]; c[i] = t;
// Intent: scalar renaming
// Dependences: confused
// Desired outcome: left as a loop.
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
