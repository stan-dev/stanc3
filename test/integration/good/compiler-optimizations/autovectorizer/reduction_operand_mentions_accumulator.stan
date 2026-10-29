// The added operand `s10 * a[n]` mentions the accumulator s10, so the update
// is a genuine recurrence and not a reduction.
// An accumulator `s` whose only accesses in the loop are increments `s += e`,
// with `e` free of `s`, is a reduction, so the increments can become one
// vector statement `s += sum(...)`.
// Dependences: the read of s10 in the operand gives the update a confused
//   edge {<,=,>}
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N] a;
}
model {
  real s10 = 1;
  for (n in 1 : N) {
    s10 = s10 + s10 * a[n];
  }
  target += s10;
}
