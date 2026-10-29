// The loop adds `y[i, x[i]] ~ std_normal()`. The first index is the loop
// variable and the second index `x[i]` also changes with `i`.
// Desired outcome: left as a loop.
// The density is not rewritten because only one index may vary with the loop
// variable and the rest must be loop-invariant.
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=N> x;
}
parameters {
  array[N] vector[N] y;
}
model {
  for (i in 1 : N) {
    y[i, x[i]] ~ std_normal();
  }
}
