// A recurrence on A next to a scalar sum `total4 += d[n]` that is used after
// the loop.
// Source: LLVM test/Transforms/LoopDistribute/outside-use.ll, translated to
//   1-based Stan.
// C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; sum += C[i]; }
// Dependences: true S1 self {Lt, 1}; total4 is a reduction accumulator, so S2
//   carries no edge
// Desired outcome: partially vectorized, S1 stays in a loop, then
//   `total4 += sum(d[1:N])`.
// LLVM distributes the loop and merges the sum reduction with a phi.
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] d;
}
model {
  vector[N + 1] a4;
  real total4 = 0;
  for (n in 1 : N) {
    a4[n + 1] = a4[n] * b[n];
    total4 += d[n];
  }
  target += sum(a4) + total4;
}
