// TSVC "StatementReordering" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Translated to
// 1-based Stan with loop variable `n`; array sizes chosen so every
// subscript is in range. Written variables are model-block locals
// initialised from data. Each example sits in its own block, and every local
// it writes carries the example number as a suffix (a2 belongs to example 2)
// so the generated MIR and C++ can be matched to the example.
data {
  int<lower=3> N;
  vector[N + 1] a0;
  vector[N + 1] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  // 1. TSVC s211 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D-1; i++) { a[i] = b[i - 1] + c[i] * d[i]; b[i] = b[i + 1] - e[i] * d[i]; }
  // Intent: vectorizable after statement reordering.
  // Design: S2 self anti-dependence: no edge; true S2->S1 {Lt, 1}.
  // Emitted: vec S2 `b1[2:(N-1)] = b1[3:N] - e .* d`, then vec S1
  // `a1[2:(N-1)] = b1[1:(N-2)] + c .* d` (reordered).
  {
    vector[N + 1] a1 = a0;
    vector[N + 1] b1 = b0;
    for (n in 2 : (N - 1)) {
      a1[n] = b1[n - 1] + c[n] .* d[n];
      b1[n] = b1[n + 1] - e[n] .* d[n];
    }
    target += sum(a1);
    target += sum(b1);
  }

  // 2. TSVC s212 (StatementReordering), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D-1; i++) { a[i] *= c[i]; b[i] += a[i + 1] * d[i]; }
  // Intent: "dependency needing temporary".
  // Design: anti S2->S1 {Lt, 1}, acyclic.
  // Emitted: vec S2 `b2[:] = b2 + a2[2:(N+1)] .* d`, then vec S1
  // `a2[:] = a2 .* c` (reordering replaces the temporary).
  {
    vector[N + 1] a2 = a0;
    vector[N + 1] b2 = b0;
    for (n in 1 : N) {
      a2[n] = a2[n] .* c[n];
      b2[n] = b2[n] + a2[n + 1] .* d[n];
    }
    target += sum(a2);
    target += sum(b2);
  }
}
