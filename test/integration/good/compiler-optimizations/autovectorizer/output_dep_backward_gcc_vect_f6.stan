// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-3.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 4. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f6
// C: for (i = 0; i < N; i++) { ia[i] = x[i]; ia[i+1] = z[i]; }
// GCC: not vectorized.
// Design: output dependence S2->S1 {Lt, 1}.
// Emitted: vec ia4[2:(N+1)] = z, then vec ia4[1:N] = x (reordered).
data {
  int<lower=1> N;
  vector[N] x;
  vector[N] z;
}
model {
  vector[N + 1] ia4;
  for (n in 1 : N) {
    ia4[n] = x[n];
    ia4[n + 1] = z[n];
  }
  target += sum(ia4);
}
