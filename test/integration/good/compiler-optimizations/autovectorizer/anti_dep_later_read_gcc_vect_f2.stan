// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-3.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 2. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f2
// C: for (i = 0; i < N; i++) { ia[i] = x[i]; ib[i] = ia[i+1]; }
// GCC: not vectorized (positive distance).
// Design: anti dependence S2->S1 {Lt, 1}, acyclic.
// Emitted: vec ib2[1:N] = ia2[2:(N+1)], then vec ia2[1:N] = x (reordered,
// Allen and Kennedy 1987 p. 508).
data {
  int<lower=1> N;
  vector[N] x;
}
model {
  vector[N + 1] ia2;
  vector[N + 1] ib2;
  for (n in 1 : N) {
    ia2[n] = x[n];
    ib2[n] = ia2[n + 1];
  }
  target += sum(ia2) + sum(ib2);
}
