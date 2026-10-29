// An elementwise product of two vectors with no dependence between
// iterations.
// Source: LLVM test/Transforms/LoopDistribute/diagnostics.ll (`not_forced`),
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) A[i] = B[i] * C[i];
// Dependences: none
// Desired outcome: fully vectorized, `a1[1:N] = b .* d`.
// LLVM does not distribute the loop because "memory operations are safe for
// vectorization" and there is nothing to isolate.
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] d;
}
model {
  vector[N] a1;
  for (n in 1 : N) {
    a1[n] = b[n] * d[n];
  }
  target += sum(a1);
}
