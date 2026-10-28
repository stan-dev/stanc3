// TSVC "InductionVariable" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 5. TSVC s121 (InductionVariable), UoB-HPC/TSVC_2 src/tsvc.c
// C: j = 1; for (int i = 0; i < LEN_1D-1; i++) { j++; a[i] = a[j] + b[i]; }
// Intent: induction variable ambiguity.
// Design: j5 is written in the body, so a5[j5] is Varying -> seq.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b;
}
model {
  vector[N + 1] a5 = a0;
  int j5 = 1;
  for (n in 1 : (N - 1)) {
    a5[n] = a5[j5] + b[n];
    j5 += 1;
  }
  target += sum(a5);
}
