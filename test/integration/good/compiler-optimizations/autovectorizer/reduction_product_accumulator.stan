// A product accumulator `prod14 *= a[n]`.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Desired outcome: left as a loop.
// Only `+=` and `-=` updates are treated as reductions.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real prod14 = 1;
  for (n in 1 : N) {
    prod14 *= a[n];
  }
  target += prod14;
}
