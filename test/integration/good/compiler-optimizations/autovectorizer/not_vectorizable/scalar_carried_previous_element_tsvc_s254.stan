// A scalar `x3` carries `b[n]` into the next iteration, where `x3` is read
// before being overwritten. Because the whole-variable access to `x3`
// (subs = []) is confused, the loop stays as written until scalar expansion
// is implemented.
// Source: TSVC s254 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = (b[i] + x) * 0.5; x = b[i];
// Intent: carry-around variable
// Dependences: confused
// Desired outcome: left as a loop.
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
