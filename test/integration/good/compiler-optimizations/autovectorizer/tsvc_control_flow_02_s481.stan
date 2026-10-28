// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `IfElse` statements are single sequential leaves of
// the loop dependence graph (no if-conversion, roadmap item 6); effectful
// statements (`reject`) stay sequential but do not stop independent
// neighbours from being hoisted.
// Outcome on feat/loop-vectorization-dependence: Partially vectorized
// 2. TSVC s481 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: if (d[i] < 0.) exit(0); a[i] += b[i] * c[i];
// intent: non-local goto (exit)
// graph: S1 `IfElse` leaf with effect; S2 isolated   emitted: S1 seq, S2 vec
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
