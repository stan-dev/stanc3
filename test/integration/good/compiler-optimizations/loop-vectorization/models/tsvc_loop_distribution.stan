// TSVC "LoopDistribution" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` is a local wherever it is written). Each example sits in
// its own block, and every local it writes carries the example number as a
// suffix (a2 belongs to example 2) so the generated MIR and C++ can be
// matched to the example. Every loop here is partially vectorized.
data {
  int<lower=2> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] e0;
  vector[N] c;
  vector[N] d;
}
model {
  // ---- Partially vectorized: some statements hoist, others stay in a loop ----

  // 1. TSVC s221 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D; i++) { a[i] += c[i] * d[i]; b[i] = b[i - 1] + a[i] + d[i]; }
  // Intent: partially recursive loop.
  // Design: S1->S2 {Eq}; S2 self {Lt, 1}.
  // Emitted: S1 vec `a1[2:N] = a1[2:N] + c[2:N] .* d[2:N]`, then S2 seq.
  {
    vector[N] a1 = a0;
    vector[N] b1 = b0;
    for (n in 2 : N) {
      a1[n] = a1[n] + c[n] .* d[n];
      b1[n] = b1[n - 1] + a1[n] + d[n];
    }
    target += sum(a1);
    target += sum(b1);
  }

  // 2. TSVC s222 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D; i++) { a[i] += b[i] * c[i]; e[i] = e[i - 1] * e[i - 1]; a[i] -= b[i] * c[i]; }
  // Intent: recurrence in the middle of the loop.
  // Design: S1->S3 {Eq}; S2 self {Lt, 1}.
  // Emitted: S1 vec, S2 seq, S3 vec.
  {
    vector[N] a2 = a0;
    vector[N] b2 = b0;
    vector[N] e2 = e0;
    for (n in 2 : N) {
      a2[n] = a2[n] + b2[n] .* c[n];
      e2[n] = e2[n - 1] * e2[n - 1];
      a2[n] = a2[n] - b2[n] .* c[n];
    }
    target += sum(a2);
    target += sum(e2);
  }
}
