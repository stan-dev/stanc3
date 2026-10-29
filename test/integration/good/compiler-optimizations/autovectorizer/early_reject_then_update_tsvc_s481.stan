// An early `reject` under an if, followed by an independent update of `a2`.
// The `IfElse` statement is a single sequential leaf of the loop dependence
// graph because the pass does not do if-conversion. The `IfElse` stays in the
// loop and the independent update still becomes a vector statement.
// Source: TSVC s481 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (d[i] < 0.) exit(0); a[i] += b[i] * c[i];
// Intent: non-local goto (exit)
// Dependences: S1 `IfElse` leaf with effect; S2 isolated
// Desired outcome: partially vectorized. S1 stays in a loop and S2 becomes a
//   vector statement.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a2 = a0;
  for (n in 1 : N) {
    if (d[n] < 0) reject("neg");
    a2[n] = a2[n] + b0[n] .* c[n];
  }
  target += sum(a2);
}
