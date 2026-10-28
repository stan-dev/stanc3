// Loop from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-1.c, translated to 1-based Stan.
// GCC's ldist pass has a cache-reuse cost model and declines to split several
// legal cases; agreement with this design is expected on legality, not on
// policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 1. GCC gcc.dg/tree-ssa/ldist-1.c
// C: for (i = 0; i < N; i++) { mya[i] = ia[i]*oxa[i] + ib[i]*oxb[i];
//      myb[i] = -ia[i]*oxb[i] + ib[i]*oxa[i];
//      oya[i] = mya[i] >> 10; oyb[i] = myb[i] >> 10; }
// GCC: legal to distribute, not split (cost model).
// Design: {Eq} edges only.
// Emitted: four vec statements.
data {
  int<lower=1> N;
  vector[N] ia;
  vector[N] ib;
  vector[N] oxa;
  vector[N] oxb;
}
model {
  vector[N] mya1;
  vector[N] myb1;
  vector[N] oya1;
  vector[N] oyb1;
  for (n in 1 : N) {
    mya1[n] = ia[n] .* oxa[n] + ib[n] .* oxb[n];
    myb1[n] = -ia[n] .* oxb[n] + ib[n] .* oxa[n];
    oya1[n] = mya1[n] / 1024;
    oyb1[n] = myb1[n] / 1024;
  }
  target += sum(mya1) + sum(myb1) + sum(oya1) + sum(oyb1);
}
