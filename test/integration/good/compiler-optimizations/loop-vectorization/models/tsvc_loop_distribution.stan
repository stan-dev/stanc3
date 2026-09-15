// TSVC "LoopDistribution" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data; each row is in its own block so the table's
// variable names can be used verbatim (Stan does not allow a local to
// shadow a data variable, so `b` is a local wherever it is written).
data {
  int<lower=2> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] e0;
  vector[N] c;
  vector[N] d;
}
model {
  // TSVC s221 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D; i++) { a[i] += c[i] * d[i]; b[i] = b[i - 1] + a[i] + d[i]; }
  // Intent: partially recursive loop.
  // Design: S1->S2 {Eq}; S2 self {Lt, 1}.
  // Emitted: S1 vec `a[2:N] = a[2:N] + c[2:N] .* d[2:N]`, then S2 seq.
  {
    vector[N] a = a0;
    vector[N] b = b0;
    for (n in 2 : N) {
      a[n] = a[n] + c[n] .* d[n];
      b[n] = b[n - 1] + a[n] + d[n];
    }
    target += sum(a);
    target += sum(b);
  }

  // TSVC s222 (LoopDistribution), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D; i++) { a[i] += b[i] * c[i]; e[i] = e[i - 1] * e[i - 1]; a[i] -= b[i] * c[i]; }
  // Intent: recurrence in the middle of the loop.
  // Design: S1->S3 {Eq}; S2 self {Lt, 1}.
  // Emitted: S1 vec, S2 seq, S3 vec.
  {
    vector[N] a = a0;
    vector[N] b = b0;
    vector[N] e = e0;
    for (n in 2 : N) {
      a[n] = a[n] + b[n] .* c[n];
      e[n] = e[n - 1] * e[n - 1];
      a[n] = a[n] - b[n] .* c[n];
    }
    target += sum(a);
    target += sum(e);
  }
}
