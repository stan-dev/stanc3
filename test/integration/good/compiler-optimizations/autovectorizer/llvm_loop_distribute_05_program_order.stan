// Loop from LLVM's loop distribution test
// llvm/test/Transforms/LoopDistribute/program-order.ll, translated to 1-based
// Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. LLVM LoopDistribute/program-order.ll
// C: for (i = 0; i < N; i++) { d = D[i]; A[i+1] = A[i] * B[i]; C[i] = d * E[i]; }
// LLVM: not distributed ("does not allow us to reorder memory
// operations").
// Design: S1<->S3 confused (scalar dd5), S2 self; blocks {S1, S3}, {S2}
// fuse.
// Emitted: loop unchanged.
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] e;
  vector[N] D;
}
model {
  vector[N + 1] a5;
  vector[N] c5;
  for (n in 1 : N) {
    real dd5 = D[n];
    a5[n + 1] = a5[n] * b[n];
    c5[n] = dd5 * e[n];
  }
  target += sum(a5) + sum(c5);
}
