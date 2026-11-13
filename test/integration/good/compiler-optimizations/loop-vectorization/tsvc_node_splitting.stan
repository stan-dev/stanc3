// TSVC "NodeSplitting" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data (Stan does not allow a local to shadow a data
// variable, so `b` and `d` are locals wherever they are written). Each
// example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example. Examples are ordered by outcome:
// fully vectorized loops first, then loops the pass leaves unchanged.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
  vector[N] e;
}
model {
  // ---- Fully vectorized: every statement becomes a vector statement ----

  // 1. TSVC s2244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i+1] = b[i] + e[i]; a[i] = b[i] + c[i]; }
  // Intent: vectorizable (the output dependence points forward).
  // Design: output S1->S2 {Lt, 1}.
  // Emitted: vec `a1[2:(N+1)] = b1 + e`, vec `a1[:] = b1 + c` (= GCC f5).
  {
    vector[N + 1] a1 = a0;
    vector[N] b1 = b0;
    for (n in 1 : N) {
      a1[n + 1] = b1[n] + e[n];
      a1[n] = b1[n] + c[n];
    }
    target += sum(a1);
  }

  // ---- Left unchanged: no statement can be hoisted ----

  // 2. TSVC s241 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] * c[i] * d[i]; b[i] = a[i] * a[i+1] * d[i]; }
  // Intent: needs preloading of a[i+1] (node splitting).
  // Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq (correct
  // without node splitting).
  {
    vector[N + 1] a2 = a0;
    vector[N] b2 = b0;
    vector[N] d2 = d0;
    for (n in 1 : N) {
      a2[n] = b2[n] .* c[n] .* d2[n];
      b2[n] = a2[n] .* a2[n + 1] .* d2[n];
    }
    target += sum(a2);
    target += sum(b2);
  }

  // 3. TSVC s243 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * d[i]; b[i] = a[i] + d[i] * e[i]; a[i] = b[i] + a[i+1] * d[i]; }
  // Intent: false dependence cycle breaking.
  // Design: S3 true/anti self and S3->S1 {Lt, 1}, S1->S2->S3 {Eq}: cycle
  // -> seq.
  {
    vector[N + 1] a3 = a0;
    vector[N] b3 = b0;
    vector[N] d3 = d0;
    for (n in 1 : N) {
      a3[n] = b3[n] + c[n] .* d3[n];
      b3[n] = a3[n] + d3[n] .* e[n];
      a3[n] = b3[n] + a3[n + 1] .* d3[n];
    }
    target += sum(a3);
    target += sum(b3);
  }

  // 4. TSVC s244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; ++i) { a[i] = b[i] + c[i] * d[i]; b[i] = c[i] + b[i]; a[i+1] = b[i] + a[i+1] * d[i]; }
  // Intent: false dependence cycle breaking.
  // Design: output S3->S1 {Lt, 1}, S1->S2 {Eq}, S2->S3 {Eq}: cycle -> seq.
  {
    vector[N + 1] a4 = a0;
    vector[N] b4 = b0;
    vector[N] d4 = d0;
    for (n in 1 : N) {
      a4[n] = b4[n] + c[n] .* d4[n];
      b4[n] = c[n] + b4[n];
      a4[n + 1] = b4[n] + a4[n + 1] .* d4[n];
    }
    target += sum(a4);
    target += sum(b4);
  }

  // 5. TSVC s1244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * c[i] + b[i]*b[i] + c[i]; d[i] = a[i] + a[i+1]; }
  // Intent: cycle with true and anti dependency.
  // Design: S1->S2 {Eq}, anti S2->S1 {Lt, 1}: cycle -> seq.
  {
    vector[N + 1] a5 = a0;
    vector[N] b5 = b0;
    vector[N] d5 = d0;
    for (n in 1 : N) {
      a5[n] = b5[n] + c[n] .* c[n];
      d5[n] = a5[n] + a5[n + 1];
    }
    target += sum(a5);
    target += sum(d5);
  }
}
