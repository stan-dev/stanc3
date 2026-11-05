// Each iteration reads a11[n + 1] before a later iteration overwrites that
// element. This is a self anti-dependence. Because a self anti-dependence does
// not add an edge, the statement becomes a vector statement. Direction sets
// are for the loop over n.
// Dependences: self anti-dependence, no edge
// Desired outcome: fully vectorized.
//   `a11[1:(N-1)] = a11[2:N] + 1`
// A deep_copy of the right-hand side makes the vector statement safe.
data {
  int<lower=2> N;
  vector[N] a0;
}
model {
  vector[N] a11 = a0;
  for (n in 1 : (N - 1)) {
    a11[n] = a11[n + 1] + 1;
  }
  target += sum(a11);
}
