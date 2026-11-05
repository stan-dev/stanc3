// Each iteration reads A[i+1] before a later iteration overwrites it, an
// anti self dependence on a later element.
// Source: LLVM test/Transforms/LoopVectorize/memdep.ll (`f1_vec`), translated
//   to 1-based Stan.
// C: for (i = 0; i < N; i++) A[i] = A[i+1] + 1;
// Dependences: anti S1 self, a read of a later iteration, does not carry an
//   edge
// Desired outcome: fully vectorized, `a1[1:N] = a1[2:(N+1)] + 1`.
// LLVM vectorizes the loop ("no plausible dependence").
data {
  int<lower=1> N;
}
model {
  vector[N + 1] a1;
  for (n in 1 : N) {
    a1[n] = a1[n + 1] + 1;
  }
  target += sum(a1);
}
