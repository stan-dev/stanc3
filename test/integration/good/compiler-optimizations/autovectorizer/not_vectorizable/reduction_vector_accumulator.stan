// A vector accumulator acc13 that sums the rows of an array.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`.
// Desired outcome: left as a loop.
// Only scalar accumulators are treated as reductions.
data {
  int<lower=2> N;
  int<lower=1> K;
  array[N] vector[K] rows;
}
model {
  vector[K] acc13 = rep_vector(0, K);
  for (n in 1 : N) {
    acc13 += rows[n];
  }
  target += sum(acc13);
}
