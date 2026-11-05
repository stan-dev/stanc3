// The inner loop over j holds a recurrence along a row of A next to an
// independent elementwise product.
// Source: LLVM test/Transforms/LoopDistribute/doubly-nested.ll, translated to
//   1-based Stan (inner loop over j).
// C: for (i) for (j = 0; j < M; j++) { A[i][j+1] = A[i][j] + B[i][j] + C[i][j];
//      D[i][j] = E[i][j] * F[i][j]; }
// Dependences: true S1 self {Lt, 1}; S2 has no dependences
// Desired outcome: partially vectorized, S1 stays in a loop and S2 becomes
//   `md3[i, 1:J] = me[i, 1:J] .* mf[i, 1:J]`.
// The outer loop over i stays a loop. Its body is now the sequential inner
// loop and a vector statement whose index is already a slice.
// LLVM distributes the inner loop.
data {
  int<lower=1> N;
  int<lower=1> J;
  matrix[N, J] mb;
  matrix[N, J] mc;
  matrix[N, J] me;
  matrix[N, J] mf;
}
model {
  matrix[N, J + 1] ma3;
  matrix[N, J] md3;
  for (i in 1 : N) {
    for (j in 1 : J) {
      ma3[i, j + 1] = ma3[i, j] + mb[i, j] + mc[i, j];
      md3[i, j] = me[i, j] * mf[i, j];
    }
  }
  target += sum(ma3) + sum(md3);
}
