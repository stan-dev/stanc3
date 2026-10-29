// The outer loop adds `x[i] ~ std_normal()` and an inner loop adds
// `y[i] ~ std_normal()` and then prints `target()`. Merging the densities would
// change the value of `target()` that each print sees.
// Desired outcome: left as a loop.
// A read of `target()` anywhere in a loop, including a nested loop, prevents
// widening of all densities in that loop. Both loops therefore stay.
data {
  int<lower=0> N;
  vector[N] x;
  row_vector[N] y;
}
model {
  for (i in 1 : N) {
    x[i] ~ std_normal();
    for (j in 1 : N) {
      y[i] ~ std_normal();
      print(target());
    }
  }
}
