// Loop from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 2. LLVM LoopVectorize/memdep.ll f2_novec
// C: for (i = 0; i < N; i++) A[i+1] = A[i] + 1;
// LLVM: not vectorized (dependence distance 1).
// Design: true self dependence {Lt, 1}: recurrence.
// Emitted: seq (loop unchanged).
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
