// The first statement writes A[i+8] and the second reads A[i], a forward
// true dependence with distance 8 that does not form a cycle.
// Source: LLVM test/Analysis/LoopAccessAnalysis/forward-loop-carried.ll,
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) { A[i+8] = B[i] + 2; C[i] = A[i] * 2; }
// Dependences: true S1->S2 {Lt, 8}
// Desired outcome: fully vectorized, `a1[9:(N+8)] = b + 2` then
//   `c1[1:N] = a1[1:N] * 2`.
// LLVM reports a forward dependence and "Memory dependences are safe".
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
