// Loops from LLVM's loop distribution tests,
// llvm/test/Transforms/LoopDistribute/{basic,diagnostics,program-order,
// outside-use,doubly-nested}.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
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
  vector[N + 1] a;
  vector[N] c;
  real total = 0;

  // LLVM LoopDistribute/basic.ll and diagnostics.ll `success`
  // C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; C[i] = D[i] * E[i]; }
  // LLVM: distributed into an unsafe loop {S1} and a safe loop {S2}.
  // Design: S1 self {Lt, 1}; S2 isolated.
  // Emitted: S1 seq, S2 vec (order by position).
  for (n in 1 : N) {
    a[n + 1] = a[n] * b[n];
    c[n] = d[n] .* e[n];
  }

  // LLVM LoopDistribute/program-order.ll
  // C: for (i = 0; i < N; i++) { d = D[i]; A[i+1] = A[i] * B[i]; C[i] = d * E[i]; }
  // LLVM: not distributed ("does not allow us to reorder memory
  // operations").
  // Design: S1<->S3 confused (scalar d), S2 self; blocks {S1, S3}, {S2}
  // fuse.
  // Emitted: loop unchanged.
  for (n in 1 : N) {
    real dd = D[n];
    a[n + 1] = a[n] * b[n];
    c[n] = dd * e[n];
  }

  // LLVM LoopDistribute/outside-use.ll
  // C: for (i = 0; i < N; i++) { A[i+1] = A[i] * B[i]; sum += C[i]; }
  // LLVM: distributed, the sum reduction merged by a phi.
  // Design: S2 self confused (total has subs = []); scalar reductions are
  // not recognised (only target += is, section 7.4).
  // Emitted: loop unchanged.
  for (n in 1 : N) {
    a[n + 1] = a[n] * b[n];
    total += c[n];
  }

  // LLVM LoopDistribute/doubly-nested.ll (inner loop over j)
  // C: for (i) for (j = 0; j < M; j++) { A[i][j+1] = A[i][j] + B[i][j] + C[i][j];
  //      D[i][j] = E[i][j] * F[i][j]; }
  // LLVM: distributed.
  // Design: S1 self {Lt, 1}; S2 isolated.
  // Emitted: S1 seq, vec d[i, 1:J] = e[i, 1:J] .* f[i, 1:J].
  {
    matrix[N, J + 1] ma;
    matrix[N, J] md;
    for (i in 1 : N) {
      for (j in 1 : J) {
        ma[i, j + 1] = ma[i, j] + mb[i, j] + mc[i, j];
        md[i, j] = me[i, j] * mf[i, j];
      }
    }
    target += sum(ma) + sum(md);
  }

  // LLVM LoopDistribute/diagnostics.ll `not_forced`
  // C: for (i = 0; i < N; i++) A[i] = B[i] * C[i];
  // LLVM: not distributed ("memory operations are safe for
  // vectorization", nothing to isolate).
  // Design: no edges.
  // Emitted: vec a[1:N] = b .* c.
  for (n in 1 : N) {
    a[n] = b[n] * c[n];
  }

  target += sum(a) + sum(c) + total;
  mu ~ normal(0, 1);
}
