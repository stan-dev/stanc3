// Two statements write A[i+1] and A[i], then a third reads A[i]. This gives
// a carried output dependence and two true dependences, all pointing forward,
// so none of them forms a cycle.
// Source: LLVM test/Analysis/LoopAccessAnalysis/forward-loop-independent.ll,
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) { A[i+1] = B[i] + 1; A[i] = B[i] + 2; C[i] = A[i] * 2; }
// Dependences: output S1->S2 {Lt, 1}, true S2->S3 {Eq}, true S1->S3 {Lt, 1}
// Desired outcome: fully vectorized, three vector statements in source order.
// LLVM reports three forward dependences and finds the loop safe.
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
