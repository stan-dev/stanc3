// The scalar prev4 carries the previous iteration's element of A into B[i].
// Source: LLVM test/Transforms/LoopVectorize/memdep.ll (`f6`), translated to
//   1-based Stan.
// C: for (i = 0; i < N; i++) { B[i] = tmp; tmp = A[i]; }
// Dependences: prev4 has no subscripts, so its accesses are confused in both
//   directions
// Desired outcome: left as a loop.
// LLVM does not vectorize the loop ("dependence through a phi").
data {
  int<lower=1> N;
}
model {
  vector[N] a4;
  vector[N] b4;
  real prev4 = 0;
  for (n in 1 : N) {
    b4[n] = prev4;
    prev4 = a4[n];
  }
  target += sum(b4) + prev4;
}
