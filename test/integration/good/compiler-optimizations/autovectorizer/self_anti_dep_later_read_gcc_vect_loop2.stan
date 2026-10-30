// Each iteration reads ib[i+1] before the next iteration overwrites that
// element. This is a self anti-dependence, which does not add an edge. The
// statement becomes a vector statement. Translated to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 2)
// C: for (i = 0; i < N; i++) ib[i] = ib[i+1] * 4;
// Dependences: anti self dependence on a later iteration's element, no edge
// Desired outcome: fully vectorized.
//   `ib5[1:N] = ib5[2:(N+1)] * 4`
// A deep_copy of the right-hand side makes the vector statement safe. GCC
// also vectorizes this loop (distance negative).
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
