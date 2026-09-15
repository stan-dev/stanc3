// Loops from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-1.c and no-vfa-vect-depend-3.c, translated to 1-based
// Stan. GCC vectorizes exactly the loops whose dependence distance is
// negative ("dependence distance negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
data {
  int<lower=1> N;
  vector[N] x;
  vector[N] z;
}
parameters {
  real mu;
}
model {
  vector[N + 1] ia;
  vector[N + 1] ib;

  // GCC gcc.dg/vect/no-vfa-vect-depend-3.c f1
  // C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ib[i] = ia[i]; }
  // GCC: vectorized (dependence distance negative).
  // Design: true dependence S1->S2 {Lt, 1}, acyclic.
  // Emitted: vec ia[2:(N+1)] = x; vec ib[1:N] = ia[1:N].
  for (n in 1 : N) {
    ia[n + 1] = x[n];
    ib[n] = ia[n];
  }

  // GCC gcc.dg/vect/no-vfa-vect-depend-3.c f2
  // C: for (i = 0; i < N; i++) { ia[i] = x[i]; ib[i] = ia[i+1]; }
  // GCC: not vectorized (positive distance).
  // Design: anti dependence S2->S1 {Lt, 1}, acyclic.
  // Emitted: vec ib[1:N] = ia[2:(N+1)], then vec ia[1:N] = x (reordered,
  // Allen and Kennedy 1987 p. 508).
  for (n in 1 : N) {
    ia[n] = x[n];
    ib[n] = ia[n + 1];
  }

  // GCC gcc.dg/vect/no-vfa-vect-depend-3.c f5
  // C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ia[i] = z[i]; }
  // GCC: vectorized.
  // Design: output dependence S1->S2 {Lt, 1}.
  // Emitted: vec ia[2:(N+1)] = x; vec ia[1:N] = z.
  for (n in 1 : N) {
    ia[n + 1] = x[n];
    ia[n] = z[n];
  }

  // GCC gcc.dg/vect/no-vfa-vect-depend-3.c f6
  // C: for (i = 0; i < N; i++) { ia[i] = x[i]; ia[i+1] = z[i]; }
  // GCC: not vectorized.
  // Design: output dependence S2->S1 {Lt, 1}.
  // Emitted: vec ia[2:(N+1)] = z, then vec ia[1:N] = x (reordered).
  for (n in 1 : N) {
    ia[n] = x[n];
    ia[n + 1] = z[n];
  }

  // GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 1)
  // C: for (i = 0; i < N; i++) ia[i+1] = ia[i] * 4;
  // GCC: not vectorized.
  // Design: true self dependence {Lt, 1}: recurrence.
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    ia[n + 1] = ia[n] * 4;
  }

  // GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 2)
  // C: for (i = 0; i < N; i++) ib[i] = ib[i+1] * 4;
  // GCC: vectorized (dependence distance negative).
  // Design: anti self dependence, read of a later iteration: no edge.
  // Emitted: vec ib[1:N] = ib[2:(N+1)] * 4 (deep_copy on the right-hand
  // side, section 7.11).
  for (n in 1 : N) {
    ib[n] = ib[n + 1] * 4;
  }

  target += sum(ia) + sum(ib);
  mu ~ normal(0, 1);
}
