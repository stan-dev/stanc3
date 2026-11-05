// The loop skips to the next iteration with `continue` when `x[i] < 0` and
// otherwise adds `x[i] ~ std_normal()`. The set of elements that reach the
// density depends on the data.
// Desired outcome: left as a loop.
// Like a `break`, a `continue` in the loop body keeps the whole loop as
// written.
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  for (i in 1 : N) {
    if (x[i] < 0) continue;
    x[i] ~ std_normal();
  }
}
