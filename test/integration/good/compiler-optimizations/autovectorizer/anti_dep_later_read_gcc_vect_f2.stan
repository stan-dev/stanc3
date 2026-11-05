// The second statement reads ia[i+1] before the first statement overwrites
// it in the next iteration. The anti dependence is acyclic, so both
// statements become vector statements, with the read placed before the
// write. Translated to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-3.c f2
// C: for (i = 0; i < N; i++) { ia[i] = x[i]; ib[i] = ia[i+1]; }
// Dependences: anti S2->S1 {Lt, 1}, acyclic
// Desired outcome: fully vectorized.
//   `ib2[1:N] = ia2[2:(N+1)]` then `ia2[1:N] = x`
// The statements are reordered as in Allen and Kennedy 1987 p. 508. GCC
// does not vectorize this loop (positive distance) because it never reorders
// statements.
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
