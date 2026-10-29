// An integer counter incremented by the loop-invariant value 1.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`.
// Desired outcome: fully vectorized, `count5 += (N - (1 - 1)) * 1`.
// An invariant operand becomes the iteration count times the operand, as for
// a density whose arguments are all invariant.
data {
  int<lower=2> N;
}
parameters {
  real alpha;
}
model {
  int count5 = 0;
  for (n in 1 : N) {
    count5 += 1;
  }
  target += count5 * alpha;
}
