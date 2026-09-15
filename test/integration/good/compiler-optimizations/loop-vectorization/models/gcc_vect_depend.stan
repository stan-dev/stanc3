// Loops from the GCC vectorizer test suite, gcc/testsuite/gcc.dg/vect/
// no-vfa-vect-depend-1.c and no-vfa-vect-depend-3.c, translated to 1-based
// Stan. GCC vectorizes exactly the loops whose dependence distance is
// negative ("dependence distance negative") and never reorders statements.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (ia3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example.
data {
  int<lower=1> N;
  vector[N] x;
  vector[N] z;
}
parameters {
  real mu;
}
model {
  // 1. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f1
  // C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ib[i] = ia[i]; }
  // GCC: vectorized (dependence distance negative).
  // Design: true dependence S1->S2 {Lt, 1}, acyclic.
  // Emitted: vec ia1[2:(N+1)] = x; vec ib1[1:N] = ia1[1:N].
  {
    vector[N + 1] ia1;
    vector[N + 1] ib1;
    for (n in 1 : N) {
      ia1[n + 1] = x[n];
      ib1[n] = ia1[n];
    }
    target += sum(ia1) + sum(ib1);
  }

  // 2. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f2
  // C: for (i = 0; i < N; i++) { ia[i] = x[i]; ib[i] = ia[i+1]; }
  // GCC: not vectorized (positive distance).
  // Design: anti dependence S2->S1 {Lt, 1}, acyclic.
  // Emitted: vec ib2[1:N] = ia2[2:(N+1)], then vec ia2[1:N] = x (reordered,
  // Allen and Kennedy 1987 p. 508).
  {
    vector[N + 1] ia2;
    vector[N + 1] ib2;
    for (n in 1 : N) {
      ia2[n] = x[n];
      ib2[n] = ia2[n + 1];
    }
    target += sum(ia2) + sum(ib2);
  }

  // 3. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f5
  // C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ia[i] = z[i]; }
  // GCC: vectorized.
  // Design: output dependence S1->S2 {Lt, 1}.
  // Emitted: vec ia3[2:(N+1)] = x; vec ia3[1:N] = z.
  {
    vector[N + 1] ia3;
    for (n in 1 : N) {
      ia3[n + 1] = x[n];
      ia3[n] = z[n];
    }
    target += sum(ia3);
  }

  // 4. GCC gcc.dg/vect/no-vfa-vect-depend-3.c f6
  // C: for (i = 0; i < N; i++) { ia[i] = x[i]; ia[i+1] = z[i]; }
  // GCC: not vectorized.
  // Design: output dependence S2->S1 {Lt, 1}.
  // Emitted: vec ia4[2:(N+1)] = z, then vec ia4[1:N] = x (reordered).
  {
    vector[N + 1] ia4;
    for (n in 1 : N) {
      ia4[n] = x[n];
      ia4[n + 1] = z[n];
    }
    target += sum(ia4);
  }

  // 5. GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 1)
  // C: for (i = 0; i < N; i++) ia[i+1] = ia[i] * 4;
  // GCC: not vectorized.
  // Design: true self dependence {Lt, 1}: recurrence.
  // Emitted: seq (loop unchanged).
  {
    vector[N + 1] ia5;
    for (n in 1 : N) {
      ia5[n + 1] = ia5[n] * 4;
    }
    target += sum(ia5);
  }

  // 6. GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 2)
  // C: for (i = 0; i < N; i++) ib[i] = ib[i+1] * 4;
  // GCC: vectorized (dependence distance negative).
  // Design: anti self dependence, read of a later iteration: no edge.
  // Emitted: vec ib6[1:N] = ib6[2:(N+1)] * 4 (deep_copy on the right-hand
  // side, section 7.11).
  {
    vector[N + 1] ib6;
    for (n in 1 : N) {
      ib6[n] = ib6[n + 1] * 4;
    }
    target += sum(ib6);
  }

  mu ~ normal(0, 1);
}
