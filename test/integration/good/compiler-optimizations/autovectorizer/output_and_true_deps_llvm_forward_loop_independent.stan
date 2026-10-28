// Loop from LLVM's LoopAccessAnalysis test
// llvm/test/Analysis/LoopAccessAnalysis/forward-loop-independent.ll,
// translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 2. LLVM LoopAccessAnalysis/forward-loop-independent.ll
// C: for (i = 0; i < N; i++) { A[i+1] = B[i] + 1; A[i] = B[i] + 2; C[i] = A[i] * 2; }
// LLVM: Forward x3, safe.
// Design: S1->S2 {Lt, 1} output, S2->S3 {Eq}, S1->S3 {Lt, 1}.
// Emitted: three vec statements, in order.
data {
  int<lower=1> N;
  vector[N] b;
}
model {
  vector[N + 1] a2;
  vector[N] c2;
  for (n in 1 : N) {
    a2[n + 1] = b[n] + 1;
    a2[n] = b[n] + 2;
    c2[n] = a2[n] * 2;
  }
  target += sum(a2) + sum(c2);
}
