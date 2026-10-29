// The second statement writes ia[i+1] and the first overwrites the same
// element in the next iteration. Because the output dependence is acyclic,
// both statements become vector statements, with the second statement moved
// first. Translated to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-3.c f6
// C: for (i = 0; i < N; i++) { ia[i] = x[i]; ia[i+1] = z[i]; }
// Dependences: output S2->S1 {Lt, 1}
// Desired outcome: fully vectorized.
//   `ia4[2:(N+1)] = z` then `ia4[1:N] = x`
// GCC does not vectorize this loop because GCC never reorders statements.
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
