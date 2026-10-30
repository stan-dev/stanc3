// The second statement reads a[i-1], written by the first statement in the
// previous iteration. Because the dependence is loop carried but acyclic, the
// loop distributes into two vector statements. Translated to 1-based Stan.
// Source: GCC gcc.dg/tree-ssa/ldist-2.c
// C: for (i = 1; i < N; i++) { a[i] += c[i]; b[i] = a[i-1] + 1; }
// Dependences: true S1->S2 {Lt, 1}
// Desired outcome: fully vectorized.
//   `a2[2:N] = a2[2:N] + c[2:N]` then `b2[2:N] = a2[1:(N-1)] + 1`
// GCC finds distribution legal here but does not split the loop because of
// the GCC cache-reuse cost model.
data {
  int<lower=1> N;
  vector[N] c;
}
model {
  vector[N] a2;
  vector[N] b2;
  for (n in 2 : N) {
    a2[n] = a2[n] + c[n];
    b2[n] = a2[n - 1] + 1;
  }
  target += sum(a2) + sum(b2);
}
