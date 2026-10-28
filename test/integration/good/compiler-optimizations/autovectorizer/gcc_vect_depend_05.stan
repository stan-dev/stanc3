// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-1.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
//
// 5. GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 2)
// C: for (i = 0; i < N; i++) ib[i] = ib[i+1] * 4;
// GCC: vectorized (dependence distance negative).
// Design: anti self dependence, read of a later iteration: no edge.
// Emitted: vec ib5[1:N] = ib5[2:(N+1)] * 4 (deep_copy on the right-hand
// side, section 7.11).
data {
  int<lower=1> N;
}
model {
  vector[N + 1] ib5;
  for (n in 1 : N) {
    ib5[n] = ib5[n + 1] * 4;
  }
  target += sum(ib5);
}
