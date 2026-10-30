// The loop variable `n` is used on the right-hand side as a value as well as
// in a subscript. The statement does not have a dependence edge. A loop that
// uses the loop variable as a value stays as written.
// Source: TSVC s452 (Control Flow / induction), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] = b[i] + c[i] * (real_t) (i+1);
// Intent: induction variable used as a value
// Dependences: no edge
// Desired outcome: left as a loop.
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
