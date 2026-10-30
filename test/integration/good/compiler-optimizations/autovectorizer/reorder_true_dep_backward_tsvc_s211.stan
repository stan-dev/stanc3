// The first statement reads b[i - 1], which the second statement wrote in
// the previous iteration. The loop vectorizes once the second statement is
// moved before the first. Translated to 1-based Stan, with written
// variables as model-block locals initialised from data.
// Source: TSVC s211 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D-1; i++) { a[i] = b[i - 1] + c[i] * d[i];
//      b[i] = b[i + 1] - e[i] * d[i]; }
// Dependences: true S2->S1 {Lt, 1}; S2 self anti-dependence carries no edge
// Desired outcome: fully vectorized.
//   `b1[2:(N-1)] = b1[3:N] - e .* d` then `a1[2:(N-1)] = b1[1:(N-2)] + c .* d`
data {
  int<lower=3> N;
  vector[N + 1] a0;
  vector[N + 1] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N + 1] a1 = a0;
  vector[N + 1] b1 = b0;
  for (n in 2 : (N - 1)) {
    a1[n] = b1[n - 1] + c[n] .* d[n];
    b1[n] = b1[n + 1] - e[n] .* d[n];
  }
  target += sum(a1);
  target += sum(b1);
}
