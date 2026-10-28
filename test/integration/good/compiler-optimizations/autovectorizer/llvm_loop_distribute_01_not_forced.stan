// Loop from LLVM's loop distribution test
// llvm/test/Transforms/LoopDistribute/diagnostics.ll, translated to 1-based
// Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 1. LLVM LoopDistribute/diagnostics.ll `not_forced`
// C: for (i = 0; i < N; i++) A[i] = B[i] * C[i];
// LLVM: not distributed ("memory operations are safe for
// vectorization", nothing to isolate).
// Design: no edges.
// Emitted: vec a1[1:N] = b .* d.
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] d;
}
model {
  vector[N] a1;
  for (n in 1 : N) {
    a1[n] = b[n] * d[n];
  }
  target += sum(a1);
}
