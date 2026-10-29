// The inner loop over j reads and writes v[i], the same element on every
// iteration. Each iteration depends on the one before.
// Source: LLVM test/Analysis/LoopAccessAnalysis/store-to-invariant-check1.ll,
//   translated to 1-based Stan (inner loop over j).
// C: for (i) for (j = 0; j < M; j++) v[i] = w[j] + v[i];
// Dependences: both accesses are invariant in j and touch the same element
//   every iteration, so the self dependence is confused
// Desired outcome: left as a loop.
// LLVM reports "Non vectorizable stores to invariant address".
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
