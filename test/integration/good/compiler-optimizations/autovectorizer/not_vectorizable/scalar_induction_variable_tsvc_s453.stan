// A scalar `s6` grows by 2 in each iteration and scales `b[n]`. This makes
// `s6` an induction variable. The whole-variable access to `s6` (subs = [])
// is confused and the loop stays as written.
// Source: TSVC s453 (Expansion / induction variable), UoB-HPC/TSVC_2
//   src/tsvc.c
// C: s += 2.; a[i] = s * b[i];
// Intent: induction variable recognition
// Dependences: confused
// Desired outcome: left as a loop.
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
