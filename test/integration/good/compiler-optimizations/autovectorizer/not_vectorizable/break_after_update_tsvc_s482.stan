// An update of `a6` followed by a `break` under an if. A `break` in the body
// keeps the whole loop as written.
// Source: TSVC s482 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += b[i] * c[i]; if (c[i] > b[i]) break;
// Intent: non-local goto (break)
// Dependences: the loop `breaks`
// Desired outcome: left as a loop.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
}
model {
  vector[N] a6 = a0;
  for (n in 1 : N) {
    a6[n] = a6[n] + b0[n] .* c[n];
    if (c[n] > b0[n]) break;
  }
  target += sum(a6);
}
