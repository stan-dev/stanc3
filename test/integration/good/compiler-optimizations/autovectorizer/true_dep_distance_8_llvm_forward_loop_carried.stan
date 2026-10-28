// Loop from LLVM's LoopAccessAnalysis test
// llvm/test/Analysis/LoopAccessAnalysis/forward-loop-carried.ll, translated to
// 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 1. LLVM LoopAccessAnalysis/forward-loop-carried.ll
// C: for (i = 0; i < N; i++) { A[i+8] = B[i] + 2; C[i] = A[i] * 2; }
// LLVM: Forward dependence, "Memory dependences are safe".
// Design: true dependence S1->S2 {Lt, 8}.
// Emitted: vec a1[9:(N+8)] = b + 2; vec c1[1:N] = a1[1:N] * 2.
data {
  int<lower=1> N;
  vector[N] b;
}
model {
  vector[N + 8] a1;
  vector[N] c1;
  for (n in 1 : N) {
    a1[n + 8] = b[n] + 2;
    c1[n] = a1[n] * 2;
  }
  target += sum(a1) + sum(c1);
}
