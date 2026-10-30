// The first statement writes ia[i+1] and the second overwrites the same
// element in the next iteration. The output dependence keeps the two vector
// statements in their original order. Translated to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-3.c f5
// C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ia[i] = z[i]; }
// Dependences: output S1->S2 {Lt, 1}
// Desired outcome: fully vectorized.
//   `ia3[2:(N+1)] = x` then `ia3[1:N] = z`
// GCC also vectorizes this loop.
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
