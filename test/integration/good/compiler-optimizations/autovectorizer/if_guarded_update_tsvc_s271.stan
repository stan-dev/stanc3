// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `IfElse` statements are single sequential leaves of
// the loop dependence graph (no if-conversion, roadmap item 6).
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 3. TSVC s271 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (b[i] > 0.) a[i] += b[i] * c[i];
// intent: loop with singularity handling; needs if-conversion
// graph: `IfElse` leaf   emitted: seq (roadmap item 6)
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
