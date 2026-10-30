// The second statement reads v7[n - 1], written by the first statement in
// the previous iteration. The dependence is loop carried but acyclic. The
// loop can therefore be distributed and each statement becomes a vector
// statement with shifted ranges. Direction sets are for the loop over n.
// Dependences: true a->b {Lt}, distance 1
// Desired outcome: fully vectorized.
//   `v7[2:N] = x[2:N]` then `y7[2:N] = v7[1:(N-1)]`
// The edge a->b forces the write of v7 to come first.
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] v0;
}
model {
  vector[N] v7 = v0;
  vector[N] y7 = v0;
  for (n in 2 : N) {
    v7[n] = x[n];
    y7[n] = v7[n - 1];
  }
  target += sum(v7) + sum(y7);
}
