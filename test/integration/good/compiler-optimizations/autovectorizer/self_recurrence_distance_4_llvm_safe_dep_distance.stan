// The statement writes A[i+4] from A[i], a recurrence with distance 4.
// Source: LLVM test/Analysis/LoopAccessAnalysis/safe-with-dep-distance.ll,
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) A[i+4] = A[i] * 2;
// Dependences: true S1 self {Lt, 4}
// Desired outcome: left as a loop.
// LLVM finds the loop safe up to vector width 4, but a whole-vector statement
// would read elements before the loop writes them.
data {
  int<lower=1> N;
}
model {
  vector[N + 4] a5;
  for (n in 1 : N) {
    a5[n + 4] = a5[n] * 2;
  }
  target += sum(a5);
}
