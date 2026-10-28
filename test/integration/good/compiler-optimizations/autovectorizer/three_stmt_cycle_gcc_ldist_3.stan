// Loop from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-3.c, translated to 1-based Stan.
// GCC's ldist pass has a cache-reuse cost model and declines to split several
// legal cases; agreement with this design is expected on legality, not on
// policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 3. GCC gcc.dg/tree-ssa/ldist-3.c
// C: for (i = 2; i < N-1; i++) { a[i] = k * i; b[i] = a[i-2] + k;
//      c[i] = b[i] + a[i+1]; d[i] = c[i-1] + k + i; }
// GCC: not split (statements stay fused).
// Design: S1->S2 {Lt, 2}, S2->S3 {Eq}, anti S3->S1 {Lt, 1}: cycle
// {S1, S2, S3}; S3->S4 {Lt, 1}.
// Emitted: S1-S3 seq (cycle); S4 seq (uses n as a value); blocks fuse:
// loop unchanged.
data {
  int<lower=1> N;
  int k;
}
model {
  vector[N + 1] a3;
  vector[N] b3;
  vector[N] cc3;
  vector[N] d3;
  for (n in 3 : N) {
    a3[n] = k * n;
    b3[n] = a3[n - 2] + k;
    cc3[n] = b3[n] + a3[n + 1];
    d3[n] = cc3[n - 1] + k + n;
  }
  target += sum(a3) + sum(b3) + sum(cc3) + sum(d3);
}
