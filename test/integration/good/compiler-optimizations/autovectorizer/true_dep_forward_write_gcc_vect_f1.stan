// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-3.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 1. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f1
// C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ib[i] = ia[i]; }
// GCC: vectorized (dependence distance negative).
// Design: true dependence S1->S2 {Lt, 1}, acyclic.
// Emitted: vec ia1[2:(N+1)] = x; vec ib1[1:N] = ia1[1:N].
data {
  int<lower=1> N;
  vector[N] x;
}
model {
  vector[N + 1] ia1;
  vector[N + 1] ib1;
  for (n in 1 : N) {
    ia1[n + 1] = x[n];
    ib1[n] = ia1[n];
  }
  target += sum(ia1) + sum(ib1);
}
