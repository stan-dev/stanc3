// A body-local scalar dd5, written by S1 and read by S3, ties those two
// statements together on either side of the recurrence S2.
// Source: LLVM test/Transforms/LoopDistribute/program-order.ll, translated to
//   1-based Stan.
// C: for (i = 0; i < N; i++) { d = D[i]; A[i+1] = A[i] * B[i]; C[i] = d * E[i]; }
// Dependences: S1<->S3 confused through scalar dd5, true S2 self {Lt, 1};
//   the blocks {S1, S3} and {S2} fuse
// Desired outcome: left as a loop.
// LLVM does not distribute the loop ("does not allow us to reorder memory
// operations").
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
