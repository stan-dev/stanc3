// A density accumulated into the function local lp1, the shape of most
// user-defined densities, called from the model block on a parameter vector.
// The loop is in the functions block, which the pass also visits.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Desired outcome: fully vectorized,
//   `lp1 += normal_lpdf(y[1:num_elements(y)] | mu[1:...], sigma)`.
// A density operand is widened the same way as in a `target +=` statement.
functions {
  real normal_loop1_lpdf(vector y, vector mu, real sigma) {
    real lp1 = 0;
    for (n in 1 : num_elements(y)) {
      lp1 += normal_lpdf(y[n] | mu[n], sigma);
    }
    return lp1;
  }
}
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
  vector[N] mu1 = alpha + beta * x;
  target += normal_loop1_lpdf(y | mu1, sigma);
}
