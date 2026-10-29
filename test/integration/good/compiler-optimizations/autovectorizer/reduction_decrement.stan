// A decrement `neg7 -= a[n]`, which is `neg7 = neg7 - a[n]`.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`. A decrement is treated the same way.
// Desired outcome: fully vectorized, `neg7 -= sum(a[1:N])`.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real neg7 = 0;
  for (n in 1 : N) {
    neg7 -= a[n];
  }
  target += neg7;
}
