// The loop is partially recursive. An independent update of a is followed by
// a recurrence on b that reads the new a. The update is distributed out as a
// vector statement and the recurrence stays in the loop. Translated to
// 1-based Stan, with written variables as model-block locals initialised
// from data.
// Source: TSVC s221 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 1; i < LEN_1D; i++) { a[i] += c[i] * d[i];
//      b[i] = b[i - 1] + a[i] + d[i]; }
// Dependences: S1->S2 {Eq}; S2 self {Lt, 1}
// Desired outcome: partially vectorized.
//   `a1[2:N] = a1[2:N] + c[2:N] .* d[2:N]`, then S2 stays in the loop.
data {
  int<lower=2> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a1 = a0;
  vector[N] b1 = b0;
  for (n in 2 : N) {
    a1[n] = a1[n] + c[n] .* d[n];
    b1[n] = b1[n - 1] + a1[n] + d[n];
  }
  target += sum(a1);
  target += sum(b1);
}
