// The statement writes A[i+1] from A[i], a recurrence with distance 1.
// Source: LLVM test/Transforms/LoopVectorize/memdep.ll (`f2_novec`),
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) A[i+1] = A[i] + 1;
// Dependences: true S1 self {Lt, 1}
// Desired outcome: left as a loop.
// LLVM does not vectorize the loop (dependence distance 1).
data {
  int<lower=1> N;
}
model {
  vector[N + 1] a2;
  for (n in 1 : N) {
    a2[n + 1] = a2[n] + 1;
  }
  target += sum(a2);
}
