// A plain sum of the elements of a into total3.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`.
// Desired outcome: fully vectorized, `total3 += sum(a[1:N])`.
// The sum adds the reals in a different order than the loop. Stan already
// accepts the same reassociation for `target`.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real total3 = 0;
  for (n in 1 : N) {
    total3 += a[n];
  }
  target += total3;
}
