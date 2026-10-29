// One loop holds two sampling statements, `x[i] ~ std_normal()` and
// `y[i] ~ normal(x[i], 1)`. Neither statement writes a variable. Both
// statements read `x` freely and each becomes a separate vector density.
// Desired outcome: fully vectorized.
//   `target += std_normal_lupdf(x[1:N])`
//   `target += normal_lupdf(y[1:N] | x[1:N], 1)`
data {
  int<lower=0> N;
  vector[N] y;
}
parameters {
  vector[N] x;
}
model {
  for (i in 1 : N) {
    x[i] ~ std_normal();
    y[i] ~ normal(x[i], 1);
  }
}
