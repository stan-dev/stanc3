// The outer loop body declares a vector `g` and an inner loop assigns
// `g[j] = x[j]`. The inner loop is widened by itself. The copy becomes a
// vector statement inside the outer loop. The outer loop stays because the
// outer loop body declares and prints `g`.
// Desired outcome: partially vectorized.
//   `g[1:M] = x[1:M]`
data {
  int<lower=0> N;
  int<lower=0, upper=N> M;
  vector[N] x;
}
model {
  for (i in 1 : N) {
    vector[M] g;
    for (j in 1 : M) {
      g[j] = x[j];
    }
    print(g);
  }
}
