// Loop from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. LLVM LoopVectorize/memdep.ll f6
// C: for (i = 0; i < N; i++) { B[i] = tmp; tmp = A[i]; }
// LLVM: not vectorized ("dependence through a phi").
// Design: prev4 has subs = []: confused in both directions.
// Emitted: seq (loop unchanged).
data {
  int<lower=1> N;
}
model {
  vector[N] a4;
  vector[N] b4;
  real prev4 = 0;
  for (n in 1 : N) {
    b4[n] = prev4;
    prev4 = a4[n];
  }
  target += sum(b4) + prev4;
}
