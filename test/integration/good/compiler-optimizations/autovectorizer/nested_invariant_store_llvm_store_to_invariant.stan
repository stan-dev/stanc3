// Loop from LLVM's LoopAccessAnalysis test
// llvm/test/Analysis/LoopAccessAnalysis/store-to-invariant-check1.ll,
// translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. LLVM LoopAccessAnalysis/store-to-invariant-check1.ll (inner loop
// over j)
// C: for (i) for (j = 0; j < M; j++) v[i] = w[j] + v[i];
// LLVM: "Non vectorizable stores to invariant address".
// Design: Invariant vs Invariant, same element every iteration: self
// confused.
// Emitted: seq (loop unchanged).
data {
  int<lower=1> N;
  int<lower=1> J;
  vector[J] w;
}
model {
  vector[N] v4;
  for (i in 1 : N) {
    for (j in 1 : J) {
      v4[i] = w[j] + v4[i];
    }
  }
  target += sum(v4);
}
