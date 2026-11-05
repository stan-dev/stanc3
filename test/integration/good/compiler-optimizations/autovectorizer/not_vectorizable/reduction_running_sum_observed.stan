// The running sum s9 is read by another statement in the body, the analogue
// of reading target() inside the loop.
// An accumulator `s` whose only accesses in the loop are increments `s += e`
// is a reduction. The increments of a reduction can become one vector
// statement `s += sum(...)`.
// Dependences: the read of s9 gives the increment a confused edge {<,=,>}
// Desired outcome: left as a loop.
// Every access to s9 must stay in order.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real s9 = 0;
  vector[N] z9;
  for (n in 1 : N) {
    s9 += a[n];
    z9[n] = s9;
  }
  target += sum(z9);
}
