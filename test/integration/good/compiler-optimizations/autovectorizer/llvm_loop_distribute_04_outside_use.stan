// Loop from LLVM's loop distribution test
// llvm/test/Transforms/LoopDistribute/outside-use.ll, translated to 1-based
// Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
// 4. LLVM LoopDistribute/outside-use.ll
// C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; sum += C[i]; }
// LLVM: distributed, the sum reduction merged by a phi.
// Design: S1 self {Lt, 1}; total4 is an accumulator (reduction, section
// 7.9 item 5), so S2 carries no edge.
// Emitted: S1 seq, then vec total4 += sum(d[1:N]).
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] d;
}
model {
  vector[N + 1] a4;
  real total4 = 0;
  for (n in 1 : N) {
    a4[n + 1] = a4[n] * b[n];
    total4 += d[n];
  }
  target += sum(a4) + total4;
}
