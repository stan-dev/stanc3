// Loop from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-1.c, translated to 1-based Stan. GCC vectorizes exactly
// the loops whose dependence distance is negative ("dependence distance
// negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
//
// 6. GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 1)
// C: for (i = 0; i < N; i++) ia[i+1] = ia[i] * 4;
// GCC: not vectorized.
// Design: true self dependence {Lt, 1}: recurrence.
// Emitted: seq (loop unchanged).
data {
  int<lower=1> N;
}
model {
  vector[N + 1] ia6;
  for (n in 1 : N) {
    ia6[n + 1] = ia6[n] * 4;
  }
  target += sum(ia6);
}
