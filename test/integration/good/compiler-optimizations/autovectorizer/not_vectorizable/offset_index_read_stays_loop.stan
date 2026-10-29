// An assignment loop `w[n] = x[n - 1]` that reads the data vector at an
// offset from the loop variable.
// Desired outcome: left as a loop.
// The subscript `n - 1` uses the loop variable inside arithmetic, which the
// vectorizer cannot turn into a range or an index array.
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  vector[N] w;
  for (n in 2 : N) {
    w[n] = x[n - 1];
  }
  target += normal_lpdf(w | 0, 1);
}
