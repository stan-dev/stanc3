// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-3.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 3. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f5
// C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ia[i] = z[i]; }
// GCC: vectorized.
// Design: output dependence S1->S2 {Lt, 1}.
// Emitted: vec ia3[2:(N+1)] = x; vec ia3[1:N] = z.
data {
  int<lower=1> N;
  vector[N] x;
  vector[N] z;
}
model {
  vector[N + 1] ia3;
  for (n in 1 : N) {
    ia3[n + 1] = x[n];
    ia3[n] = z[n];
  }
  target += sum(ia3);
}
