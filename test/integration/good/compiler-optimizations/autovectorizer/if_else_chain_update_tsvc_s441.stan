// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `IfElse` statements are single sequential leaves of
// the loop dependence graph (no if-conversion, roadmap item 6).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 4. TSVC s441 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (d[i] < 0.) a[i] += b[i]*c[i]; else if (d[i] == 0.) a[i] += b[i]*b[i]; else a[i] += c[i]*c[i];
// intent: arithmetic if
// graph: `IfElse` leaf   emitted: seq
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a4 = a0;
  for (n in 1 : N) {
    if (d[n] < 0) a4[n] = a4[n] + b0[n] * c[n];
    else if (d[n] == 0) a4[n] = a4[n] + b0[n] * b0[n];
    else a4[n] = a4[n] + c[n] * c[n];
  }
  target += sum(a4);
}
