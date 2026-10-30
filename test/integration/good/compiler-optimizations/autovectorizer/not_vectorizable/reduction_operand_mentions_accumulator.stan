// The added operand `s10 * a[n]` mentions the accumulator s10, which makes the
// update a recurrence. The reduction rule below does not apply.
// An accumulator `s` whose only accesses in the loop are increments `s += e`,
// with `e` free of `s`, is a reduction. The increments of a reduction can
// become one vector statement `s += sum(...)`.
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
