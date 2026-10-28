// Loop from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. LLVM LoopVectorize/memdep.ll f3
// C: for (i = 0; i < N; i++) A[i+2] = A[i] + 1;
// LLVM: vectorized at width 2 only.
// Design: true self dependence {Lt, 2}; a whole-vector statement is
// illegal (agrees with "not at width >= 3").
// Emitted: seq (loop unchanged).
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
