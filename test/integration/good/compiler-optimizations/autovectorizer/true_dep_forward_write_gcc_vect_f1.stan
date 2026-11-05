// The first statement writes ia[i+1] and the second reads ia[i] in the next
// iteration. The true dependence is loop carried and acyclic, so both
// statements become vector statements in their original order. Translated
// to 1-based Stan.
// Source: GCC gcc.dg/vect/no-vfa-vect-depend-3.c f1
// C: for (i = 0; i < N; i++) { ia[i+1] = x[i]; ib[i] = ia[i]; }
// Dependences: true S1->S2 {Lt, 1}, acyclic
// Desired outcome: fully vectorized.
//   `ia1[2:(N+1)] = x` then `ib1[1:N] = ia1[1:N]`
// GCC vectorizes only loops whose dependence distance is negative and never
// reorders statements. GCC vectorizes this one (distance negative).
data {
  int<lower=1> N;
  vector[N] x;
}
model {
  vector[N + 1] ia1;
  vector[N + 1] ib1;
  for (n in 1 : N) {
    ia1[n + 1] = x[n];
    ib1[n] = ia1[n];
  }
  target += sum(ia1) + sum(ib1);
}
