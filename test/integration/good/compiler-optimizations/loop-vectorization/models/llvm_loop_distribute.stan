// Loops from LLVM's loop distribution tests,
// llvm/test/Transforms/LoopDistribute/{basic,diagnostics,program-order,
// outside-use,doubly-nested}.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example. Examples are ordered by outcome:
// fully vectorized loops first, then partially vectorized loops, then loops
// the pass leaves unchanged.
data {
  int<lower=1> N;
  int<lower=1> J;
  vector[N] b;
  vector[N] d;
  vector[N] e;
  vector[N] D;
  matrix[N, J] mb;
  matrix[N, J] mc;
  matrix[N, J] me;
  matrix[N, J] mf;
}
parameters {
  real mu;
}
model {
  // ---- Fully vectorized: every statement becomes a vector statement ----

  // 1. LLVM LoopDistribute/diagnostics.ll `not_forced`
  // C: for (i = 0; i < N; i++) A[i] = B[i] * C[i];
  // LLVM: not distributed ("memory operations are safe for
  // vectorization", nothing to isolate).
  // Design: no edges.
  // Emitted: vec a1[1:N] = b .* d.
  {
    vector[N] a1;
    for (n in 1 : N) {
      a1[n] = b[n] * d[n];
    }
    target += sum(a1);
  }

  // ---- Partially vectorized: some statements hoist, others stay in a loop ----

  // 2. LLVM LoopDistribute/basic.ll and diagnostics.ll `success`
  // C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; C[i] = D[i] * E[i]; }
  // LLVM: distributed into an unsafe loop {S1} and a safe loop {S2}.
  // Design: S1 self {Lt, 1}; S2 isolated.
  // Emitted: S1 seq, S2 vec (order by position).
  {
    vector[N + 1] a2;
    vector[N] c2;
    for (n in 1 : N) {
      a2[n + 1] = a2[n] * b[n];
      c2[n] = d[n] .* e[n];
    }
    target += sum(a2) + sum(c2);
  }

  // 3. LLVM LoopDistribute/doubly-nested.ll (inner loop over j)
  // C: for (i) for (j = 0; j < M; j++) { A[i][j+1] = A[i][j] + B[i][j] + C[i][j];
  //      D[i][j] = E[i][j] * F[i][j]; }
  // LLVM: distributed.
  // Design: S1 self {Lt, 1}; S2 isolated.
  // Emitted: S1 seq, vec md3[i, 1:J] = me[i, 1:J] .* mf[i, 1:J]. The outer
  // loop over i is left unchanged: its body is now the sequential inner loop
  // (a recurrence) and a vector statement whose index is already a slice.
  {
    matrix[N, J + 1] ma3;
    matrix[N, J] md3;
    for (i in 1 : N) {
      for (j in 1 : J) {
        ma3[i, j + 1] = ma3[i, j] + mb[i, j] + mc[i, j];
        md3[i, j] = me[i, j] * mf[i, j];
      }
    }
    target += sum(ma3) + sum(md3);
  }

  // ---- Left unchanged: no statement can be hoisted ----

  // 4. LLVM LoopDistribute/program-order.ll
  // C: for (i = 0; i < N; i++) { d = D[i]; A[i+1] = A[i] * B[i]; C[i] = d * E[i]; }
  // LLVM: not distributed ("does not allow us to reorder memory
  // operations").
  // Design: S1<->S3 confused (scalar dd4), S2 self; blocks {S1, S3}, {S2}
  // fuse.
  // Emitted: loop unchanged.
  {
    vector[N + 1] a4;
    vector[N] c4;
    for (n in 1 : N) {
      real dd4 = D[n];
      a4[n + 1] = a4[n] * b[n];
      c4[n] = dd4 * e[n];
    }
    target += sum(a4) + sum(c4);
  }

  // 5. LLVM LoopDistribute/outside-use.ll
  // C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; sum += C[i]; }
  // LLVM: distributed, the sum reduction merged by a phi.
  // Design: S2 self confused (total5 has subs = []); scalar reductions are
  // not recognised (only target += is, section 7.4).
  // Emitted: loop unchanged.
  {
    vector[N + 1] a5;
    real total5 = 0;
    for (n in 1 : N) {
      a5[n + 1] = a5[n] * b[n];
      total5 += d[n];
    }
    target += sum(a5) + total5;
  }

  mu ~ normal(0, 1);
}
