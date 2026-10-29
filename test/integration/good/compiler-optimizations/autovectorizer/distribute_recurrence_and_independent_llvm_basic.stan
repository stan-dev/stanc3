// A recurrence on A next to an independent elementwise product. The loop can
// be split so that only the recurrence stays sequential.
// Source: LLVM test/Transforms/LoopDistribute/basic.ll and diagnostics.ll
//   (`success`), translated to 1-based Stan.
// C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; C[i] = D[i] * E[i]; }
// Dependences: true S1 self {Lt, 1}; S2 has no dependences
// Desired outcome: partially vectorized, S1 stays in a loop and S2 becomes a
//   vector statement, in source order.
// LLVM distributes the loop into an unsafe loop {S1} and a safe loop {S2}.
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
