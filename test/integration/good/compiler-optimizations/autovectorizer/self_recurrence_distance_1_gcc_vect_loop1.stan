// A first-order recurrence: each iteration reads ia[i], written by the
// previous iteration. Translated to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-1.c (loop 1)
// C: for (i = 0; i < N; i++) ia[i+1] = ia[i] * 4;
// Dependences: true self dependence {Lt, 1} (recurrence)
// Desired outcome: left as a loop.
// GCC does not vectorize this loop either.
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
