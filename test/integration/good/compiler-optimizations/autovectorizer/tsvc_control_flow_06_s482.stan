// TSVC "Control Flow" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `break` bails the whole loop.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 6. TSVC s482 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += b[i] * c[i]; if (c[i] > b[i]) break;
// intent: non-local goto (break)
// graph: `breaks`   emitted: bail
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
}
model {
  vector[N] a6 = a0;
  for (n in 1 : N) {
    a6[n] = a6[n] + b0[n] .* c[n];
    if (c[n] > b0[n]) break;
  }
  target += sum(a6);
}
