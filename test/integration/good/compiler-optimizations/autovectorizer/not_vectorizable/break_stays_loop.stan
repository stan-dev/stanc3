// The loop adds `x[k] ~ std_normal()` and then leaves with `break` when
// `x[k] < 0`. The number of iterations depends on the data.
// Desired outcome: left as a loop.
// A `break` in the loop body keeps the whole loop as written.
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  for (k in 1 : N) {
    x[k] ~ std_normal();
    if (x[k] < 0) {
      break;
    }
  }
}
