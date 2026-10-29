// A dot product written as a loop.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Desired outcome: fully vectorized, `dot4 += sum(a[1:N] .* b[1:N])`.
// A partial-evaluator rule can later turn this into `dot_product`.
data {
  int<lower=2> N;
  vector[N] a;
  vector[N] b;
}
parameters {
  real alpha;
}
model {
  real dot4 = 0;
  for (n in 1 : N) {
    dot4 += a[n] * b[n];
  }
  target += dot4 * alpha;
}
