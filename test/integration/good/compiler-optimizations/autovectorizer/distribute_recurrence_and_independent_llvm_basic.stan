// Loop from LLVM's loop distribution tests
// llvm/test/Transforms/LoopDistribute/{basic,diagnostics}.ll, translated to
// 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
// 2. LLVM LoopDistribute/basic.ll and diagnostics.ll `success`
// C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; C[i] = D[i] * E[i]; }
// LLVM: distributed into an unsafe loop {S1} and a safe loop {S2}.
// Design: S1 self {Lt, 1}; S2 isolated.
// Emitted: S1 seq, S2 vec (order by position).
data {
  int<lower=1> N;
  vector[N] b;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N + 1] a2;
  vector[N] c2;
  for (n in 1 : N) {
    a2[n + 1] = a2[n] * b[n];
    c2[n] = d[n] .* e[n];
  }
  target += sum(a2) + sum(c2);
}
