// TSVC "NodeSplitting" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data; each row is in its own block so the table's
// variable names can be used verbatim (Stan does not allow a local to
// shadow a data variable, so `b` and `d` are locals wherever they are
// written).
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
  vector[N] e;
}
model {
  // TSVC s241 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] * c[i] * d[i]; b[i] = a[i] * a[i+1] * d[i]; }
  // Intent: needs preloading of a[i+1] (node splitting).
  // Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq (correct
  // without node splitting).
  {
    vector[N + 1] a = a0;
    vector[N] b = b0;
    vector[N] d = d0;
    for (n in 1 : N) {
      a[n] = b[n] .* c[n] .* d[n];
      b[n] = a[n] .* a[n + 1] .* d[n];
    }
    target += sum(a);
    target += sum(b);
  }

  // TSVC s243 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * d[i]; b[i] = a[i] + d[i] * e[i]; a[i] = b[i] + a[i+1] * d[i]; }
  // Intent: false dependence cycle breaking.
  // Design: S3 true/anti self and S3->S1 {Lt, 1}, S1->S2->S3 {Eq}: cycle
  // -> seq.
  {
    vector[N + 1] a = a0;
    vector[N] b = b0;
    vector[N] d = d0;
    for (n in 1 : N) {
      a[n] = b[n] + c[n] .* d[n];
      b[n] = a[n] + d[n] .* e[n];
      a[n] = b[n] + a[n + 1] .* d[n];
    }
    target += sum(a);
    target += sum(b);
  }

  // TSVC s244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; ++i) { a[i] = b[i] + c[i] * d[i]; b[i] = c[i] + b[i]; a[i+1] = b[i] + a[i+1] * d[i]; }
  // Intent: false dependence cycle breaking.
  // Design: output S3->S1 {Lt, 1}, S1->S2 {Eq}, S2->S3 {Eq}: cycle -> seq.
  {
    vector[N + 1] a = a0;
    vector[N] b = b0;
    vector[N] d = d0;
    for (n in 1 : N) {
      a[n] = b[n] + c[n] .* d[n];
      b[n] = c[n] + b[n];
      a[n + 1] = b[n] + a[n + 1] .* d[n];
    }
    target += sum(a);
    target += sum(b);
  }

  // TSVC s1244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * c[i] + b[i]*b[i] + c[i]; d[i] = a[i] + a[i+1]; }
  // Intent: cycle with true and anti dependency.
  // Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq.
  {
    vector[N + 1] a = a0;
    vector[N] b = b0;
    vector[N] d = d0;
    for (n in 1 : N) {
      a[n] = b[n] + c[n] .* c[n];
      d[n] = a[n] + a[n + 1];
    }
    target += sum(a);
    target += sum(d);
  }

  // TSVC s2244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i+1] = b[i] + e[i]; a[i] = b[i] + c[i]; }
  // Intent: vectorizable (the output dependence points forward).
  // Design: output S1->S2 {Lt, 1}.
  // Emitted: vec `a[2:(N+1)] = b + e`, vec `a[:] = b + c` (= GCC f5).
  {
    vector[N + 1] a = a0;
    vector[N] b = b0;
    for (n in 1 : N) {
      a[n + 1] = b[n] + e[n];
      a[n] = b[n] + c[n];
    }
    target += sum(a);
  }
}
