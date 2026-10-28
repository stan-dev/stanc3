// Loop from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-2.c, translated to 1-based Stan.
// GCC's ldist pass has a cache-reuse cost model and declines to split several
// legal cases; agreement with this design is expected on legality, not on
// policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 2. GCC gcc.dg/tree-ssa/ldist-2.c
// C: for (i = 1; i < N; i++) { a[i] += c[i]; b[i] = a[i-1] + 1; }
// GCC: legal to distribute, not split (cost model).
// Design: true dependence S1->S2 {Lt, 1}.
// Emitted: vec a2[2:N] = a2[2:N] + c[2:N]; vec b2[2:N] = a2[1:(N-1)] + 1.
data {
  int<lower=1> N;
  vector[N] c;
}
model {
  vector[N] a2;
  vector[N] b2;
  for (n in 2 : N) {
    a2[n] = a2[n] + c[n];
    b2[n] = a2[n - 1] + 1;
  }
  target += sum(a2) + sum(b2);
}
