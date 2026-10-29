// A sum over the output of a recurrence. The recurrence on v8 must stay a
// loop, and the sum of its results can follow it as a vector statement.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction, so the increments can become one vector statement
// `s += sum(...)`.
// Dependences: true S1 self {Lt, 1}, true S1->S2 {Eq}
// Desired outcome: partially vectorized,
//   `for (n in 2:N) v8[n] = v8[n - 1] + w[n]` stays a loop, then
//   `s8 += sum(v8[2:N])`.
data {
  int<lower=2> N;
  vector<lower=0>[N] w;
}
model {
  vector[N] v8 = w;
  real s8 = 0;
  for (n in 2 : N) {
    v8[n] = v8[n - 1] + w[n];
    s8 += v8[n];
  }
  target += sum(v8) + s8;
}
