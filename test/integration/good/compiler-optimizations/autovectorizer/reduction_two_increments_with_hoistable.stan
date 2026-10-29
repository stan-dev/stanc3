// Two increments of the same accumulator s6 next to an assignment to mu6
// that can become a vector statement.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Dependences: true S1->S2 {Eq}; the two increments carry no edge between
//   them
// Desired outcome: fully vectorized, `mu6[1:N] = alpha + beta * x`, then
//   `s6 += sum(mu6[1:N])`, then `s6 += sum(w[1:N])`.
data {
  int<lower=2> N;
  vector[N] x;
  vector<lower=0>[N] w;
}
parameters {
  real alpha;
  real beta;
}
model {
  vector[N] mu6;
  real s6 = 0;
  for (n in 1 : N) {
    mu6[n] = alpha + beta * x[n];
    s6 += mu6[n];
    s6 += w[n];
  }
  target += s6;
}
