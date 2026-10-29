// The statement writes A[i+2] from A[i], a recurrence with distance 2.
// Source: LLVM test/Transforms/LoopVectorize/memdep.ll (`f3`), translated to
//   1-based Stan.
// C: for (i = 0; i < N; i++) A[i+2] = A[i] + 1;
// Dependences: true S1 self {Lt, 2}
// Desired outcome: left as a loop.
// LLVM vectorizes the loop at width 2 only. A whole-vector statement is not
// legal here, which agrees with LLVM refusing widths of 3 or more.
data {
  int<lower=1> N;
}
model {
  vector[N + 2] a3;
  for (n in 1 : N) {
    a3[n + 2] = a3[n] + 1;
  }
  target += sum(a3);
}
