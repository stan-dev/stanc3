// An update of `a3` guarded by an if. An `IfElse` statement is a single
// sequential leaf of the loop dependence graph because there is no
// if-conversion, so the loop stays as written.
// Source: TSVC s271 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (b[i] > 0.) a[i] += b[i] * c[i];
// Intent: loop with singularity handling, needs if-conversion
// Dependences: `IfElse` leaf
// Desired outcome: left as a loop.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
}
model {
  vector[N] a3 = a0;
  for (n in 1 : N) {
    if (b0[n] > 0) a3[n] = a3[n] + b0[n] * c[n];
  }
  target += sum(a3);
}
