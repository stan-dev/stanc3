// The loop adds `exp(g[k]) ~ std_normal()`. The argument of the density is a
// function call, so the pass widens `g[k]` inside `exp` and passes the vector
// `exp` result to the density.
// Desired outcome: fully vectorized.
//   `target += std_normal_lupdf(exp(g[1:M]))`
data {
  int<lower=0> M;
}
parameters {
  vector[M] g;
}
model {
  for (k in 1 : M) {
    exp(g[k]) ~ std_normal();
  }
}
