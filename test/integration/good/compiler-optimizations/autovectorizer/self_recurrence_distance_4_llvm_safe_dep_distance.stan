// Loop from LLVM's LoopAccessAnalysis test
// llvm/test/Analysis/LoopAccessAnalysis/safe-with-dep-distance.ll, translated
// to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. LLVM LoopAccessAnalysis/safe-with-dep-distance.ll
// C: for (i = 0; i < N; i++) A[i+4] = A[i] * 2;
// LLVM: safe up to vector width 4.
// Design: true self dependence {Lt, 4}; a whole-vector statement is
// illegal.
// Emitted: seq (loop unchanged).
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
