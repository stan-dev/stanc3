// The loop writes and reads A through the index array idx, so which elements
// two iterations touch cannot be compared.
// Source: LLVM
//   test/Analysis/LoopAccessAnalysis/loops-with-indirect-reads-and-writes.ll,
//   translated to 1-based Stan.
// C: for (i = 0; i < N; i++) { A[idx[i]] = B[i]; C[i] = A[idx[i]]; }
// Dependences: the subscript idx[n] varies with the loop, so the dependence
//   is confused
// Desired outcome: left as a loop.
// LLVM reports IndirectUnsafe.
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
