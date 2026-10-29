// The increment of s11 is inside an if statement.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`.
// Dependences: the increment still commutes and does not carry an edge
// Desired outcome: left as a loop.
// The if statement itself is not turned into a vector statement, so the loop
// stays.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real s11 = 0;
  for (n in 1 : N) {
    if (a[n] > 0) s11 += a[n];
  }
  target += s11;
}
