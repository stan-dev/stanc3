// Loop from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-4.c, translated to 1-based Stan.
// GCC's ldist pass has a cache-reuse cost model and declines to split several
// legal cases; agreement with this design is expected on legality, not on
// policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 4. GCC gcc.dg/tree-ssa/ldist-4.c
// C: for (i = 0; i < N; i++) for (j = 1; j < M; j++)
//      { a[j] = k * i; b[i][j] = a[j-1] + k; }
// GCC: not distributed.
// Design (inner loop over j): true dependence S1->S2 {Lt, 1}.
// Emitted: inner loop over j is partially vectorized, S1 seq (invariant
// right-hand side), then vec b4[i, 2:J] = a4[1:(J-1)] + k; the outer loop
// over i is left unchanged (its body carries a recurrence through a4).
data {
  int<lower=1> N;
  int<lower=1> J;
  int k;
}
model {
  vector[J] a4;
  matrix[N, J] b4;
  for (i in 1 : N) {
    for (j in 2 : J) {
      a4[j] = k * i;
      b4[i, j] = a4[j - 1] + k;
    }
  }
  target += sum(a4) + sum(b4);
}
