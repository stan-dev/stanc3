// Loop from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 1. LLVM LoopVectorize/memdep.ll f1_vec
// C: for (i = 0; i < N; i++) A[i] = A[i+1] + 1;
// LLVM: vectorized ("no plausible dependence").
// Design: anti self dependence, read of a later iteration: no edge.
// Emitted: vec a1[1:N] = a1[2:(N+1)] + 1.
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
