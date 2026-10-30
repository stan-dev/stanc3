// A scalar temporary `s1` is written and then read in each iteration. The
// whole-variable access to `s1` (subs = []) is confused, so the loop stays
// as written until scalar expansion is implemented.
// Source: TSVC s251 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: s = b[i] + c[i] * d[i]; a[i] = s * s;
// Intent: scalar expansion, vectorizable after expansion
// Dependences: `s1` confused (subs = [])
// Desired outcome: left as a loop.
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
