// A scalar `s4` is written only when `a0[n] > 0` but read in every
// iteration, so the last written value wraps around to later iterations.
// The `IfElse` statement is a sequential leaf and the whole-variable access
// to `s4` (subs = []) is confused, so the loop stays as written.
// Source: TSVC s258 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (a[i] > 0.) s = d[i] * d[i]; b[i] = s * c[i] + d[i];
// Intent: wrap-around scalar under an if
// Dependences: `IfElse` leaf and `s4` confused
// Desired outcome: left as a loop.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  real s4 = 0;
  vector[N] b4 = b;
  for (n in 1 : N) {
    if (a0[n] > 0) s4 = d[n] * d[n];
    b4[n] = s4 * c[n] + d[n];
  }
  target += sum(b4);
}
