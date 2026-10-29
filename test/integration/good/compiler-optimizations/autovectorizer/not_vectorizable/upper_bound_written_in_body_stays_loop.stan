// The loop runs `k` from 1 to `m` and the body decrements `m` with `m -= 1`.
// A vector statement would read the upper bound once, before any decrement.
// Desired outcome: left as a loop.
// The pass keeps any loop whose body writes a variable that the upper bound
// reads.
data {
  int<lower=0> M;
}
parameters {
  vector[M] g;
}
model {
  int m = M;
  for (k in 1 : m) {
    g[k] ~ std_normal();
    m -= 1;
  }
}
