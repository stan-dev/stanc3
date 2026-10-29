// A scalar `t2` carries the temporary `s2` from one iteration into the next.
// The whole-variable access to `t2` (subs = []) is confused, so the loop
// stays as written until scalar expansion is implemented.
// Source: TSVC s252 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: s = b[i] * c[i]; a[i] = s + t; t = s;
// Intent: loop with ambiguous scalar temporary
// Dependences: confused
// Desired outcome: left as a loop.
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
