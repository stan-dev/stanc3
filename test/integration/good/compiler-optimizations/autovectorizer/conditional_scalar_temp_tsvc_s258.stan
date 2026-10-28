// TSVC "Expansion" loop (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). The loop carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. TSVC s258 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (a[i] > 0.) s = d[i] * d[i]; b[i] = s * c[i] + d[i];
// intent: wrap-around scalar under an if
// graph: `IfElse` leaf + `s4` confused   emitted: seq
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
