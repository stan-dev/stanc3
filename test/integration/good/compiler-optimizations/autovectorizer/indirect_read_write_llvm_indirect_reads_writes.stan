// Loop from LLVM's LoopAccessAnalysis test
// llvm/test/Analysis/LoopAccessAnalysis/loops-with-indirect-reads-and-writes.ll,
// translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. LLVM LoopAccessAnalysis/loops-with-indirect-reads-and-writes.ll
// C: for (i = 0; i < N; i++) { A[idx[i]] = B[i]; C[i] = A[idx[i]]; }
// LLVM: IndirectUnsafe.
// Design: Varying subscript: confused.
// Emitted: seq (loop unchanged).
data {
  int<lower=1> N;
  vector[N] b;
  array[N] int<lower=1, upper=N> idx;
}
model {
  vector[N] a3;
  vector[N] c3;
  for (n in 1 : N) {
    a3[idx[n]] = b[n];
    c3[n] = a3[idx[n]];
  }
  target += sum(a3) + sum(c3);
}
