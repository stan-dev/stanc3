// The increment adds a density and `log(w[n])` in one operand. The widened
// density already sums over its arguments while `log(w[n])` widens to a
// vector, so taking `sum` of the widened operand would be wrong.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] y;
  vector<lower=0>[N] w;
}
parameters {
  real alpha;
  real<lower=0> sigma;
}
model {
  real lp12 = 0;
  for (n in 1 : N) {
    lp12 += normal_lpdf(y[n] | alpha, sigma) + log(w[n]);
  }
  target += lp12;
}
