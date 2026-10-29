// A nested loop where the inner loop over j writes a4[j] and then reads
// a4[j - 1], written in the previous inner iteration. Every outer iteration
// rewrites a4, so the outer loop over i carries a dependence through a4.
// Translated to 1-based Stan.
// Source: GCC gcc.dg/tree-ssa/ldist-4.c
// C: for (i = 0; i < N; i++) for (j = 1; j < M; j++)
//      { a[j] = k * i; b[i][j] = a[j-1] + k; }
// Dependences (inner loop over j): true S1->S2 {Lt, 1}
// Desired outcome: left as a loop.
// The inner dependence alone would allow the inner loop to be partially
// vectorized, with S1 (invariant right-hand side) staying in the loop and
// then `b4[i, 2:J] = a4[1:(J-1)] + k`. GCC does not distribute this loop.
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
