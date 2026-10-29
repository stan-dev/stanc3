// A density accumulated into the model-block local lp2, which is then added
// to target.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Desired outcome: fully vectorized,
//   `lp2 += normal_lpdf(y | alpha + beta * x, sigma)`.
// A density operand is widened the same way as in a `target +=` statement.
data {
  int<lower=2> N;
  vector[N] x;
  vector[N] y;
}
parameters {
  real alpha;
  real beta;
  real<lower=0> sigma;
}
model {
  real lp2 = 0;
  for (n in 1 : N) {
    lp2 += normal_lpdf(y[n] | alpha + beta * x[n], sigma);
  }
  target += lp2;
}
