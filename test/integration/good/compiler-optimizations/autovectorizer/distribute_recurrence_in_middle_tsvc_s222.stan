// The loop has a recurrence on e between two independent updates of a. The
// two updates become vector statements and the recurrence stays in a loop
// between them. Translated to 1-based Stan, with written variables as
// model-block locals initialised from data.
// Source: TSVC s222 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D; i++) { a[i] += b[i] * c[i];
//      e[i] = e[i - 1] * e[i - 1]; a[i] -= b[i] * c[i]; }
// Dependences: S1->S3 {Eq}; S2 self {Lt, 1}
// Desired outcome: partially vectorized.
//   S1 becomes a vector statement, S2 stays in a loop, S3 becomes a vector
//   statement.
data {
  int<lower=2> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] e0;
  vector[N] c;
}
model {
  vector[N] a2 = a0;
  vector[N] b2 = b0;
  vector[N] e2 = e0;
  for (n in 2 : N) {
    a2[n] = a2[n] + b2[n] .* c[n];
    e2[n] = e2[n - 1] * e2[n - 1];
    a2[n] = a2[n] - b2[n] .* c[n];
  }
  target += sum(a2);
  target += sum(e2);
}
