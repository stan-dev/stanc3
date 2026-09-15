// TSVC "LinearDependence" loops (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. Each loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data. Each row is in its own block so the table's
// variable names can be used verbatim.
data {
  int<lower=2> N;
  int<lower=0> k;
  vector[N + 1] a0;
  vector[N + k] a0k;
  vector[N] b;
}
model {
  // TSVC s112 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = LEN_1D - 2; i >= 0; i--) a[i+1] = a[i] + b[i];
  // Intent: vectorizable only by loop reversal.
  // Design: true self-dependence {Lt, distance 1}: recurrence -> seq
  // (loop unchanged).
  {
    vector[N + 1] a = a0;
    for (n in 1 : N) {
      a[n + 1] = a[n] + b[n];
    }
    target += sum(a);
  }

  // TSVC s113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 1; i < LEN_1D; i++) a[i] = a[0] + b[i];
  // Intent: vectorizable (a[1] is never written by the loop).
  // Design: Affine vs Invariant subscript: confused -> seq (conservative;
  // weak-zero SIV test is roadmap item 3).
  {
    vector[N + 1] a = a0;
    for (n in 2 : N) {
      a[n] = a[1] + b[n];
    }
    target += sum(a);
  }

  // TSVC s1113 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D; i++) a[i] = a[LEN_1D/2] + b[i];
  // Intent: one real dependence at n = N/2: not vectorizable as a whole.
  // Design: confused -> seq (correct).
  {
    vector[N + 1] a = a0;
    for (n in 1 : N) {
      a[n] = a[N %/% 2] + b[n];
    }
    target += sum(a);
  }

  // TSVC s293 (LinearDependence), UoB-HPC/TSVC_2 src/tsvc.c
  // C: for (int i = 0; i < LEN_1D; i++) a[i] = a[0];
  // Intent: actual dependence cycle through a[0].
  // Design: confused -> seq (correct; contrast s113).
  {
    vector[N + 1] a = a0;
    for (n in 1 : N) {
      a[n] = a[1];
    }
    target += sum(a);
  }

  // TSVC s121 (InductionVariable), UoB-HPC/TSVC_2 src/tsvc.c
  // C: j = 1; for (int i = 0; i < LEN_1D-1; i++) { j++; a[i] = a[j] + b[i]; }
  // Intent: induction variable ambiguity.
  // Design: j is written in the body, so a[j] is Varying -> seq.
  {
    vector[N + 1] a = a0;
    int j = 1;
    for (n in 1 : (N - 1)) {
      a[n] = a[j] + b[n];
      j += 1;
    }
    target += sum(a);
  }

  // TSVC s431 (Symbolics), UoB-HPC/TSVC_2 src/tsvc.c
  // C: int k = 2*inc_1d - 1; for (int i = 0; i < LEN_1D; i++) a[i] = a[i+k] + b[i];
  // Intent: needs the value of k to decide.
  // Design: a[n] vs a[n + k]: the offsets differ by the symbol k, so the
  // pair is confused -> seq (correct). Contrast design example 12, where
  // both sides carry the same k and the symbols cancel.
  {
    vector[N + k] a = a0k;
    for (n in 1 : N) {
      a[n] = a[n + k] + b[n];
    }
    target += sum(a);
  }
}
