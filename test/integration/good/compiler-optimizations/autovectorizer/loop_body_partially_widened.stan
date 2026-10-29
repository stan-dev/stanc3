// The loop body holds `x[i] ~ std_normal()`, `a[i] = exp(x[i])`, and a print
// that the pass cannot widen. The two widenable statements move out in front
// of the loop as vector statements, and the loop keeps only the print.
// Desired outcome: partially vectorized.
//   `target += std_normal_lupdf(x[1:N])`
//   `a[1:N] = exp(x[1:N])`
data {
  int<lower=0> N;
  vector[N] x;
}
model {
  vector[N] a;
  for (i in 1 : N) {
    x[i] ~ std_normal();
    a[i] = exp(x[i]);
    print(x[i]);
  }
}
