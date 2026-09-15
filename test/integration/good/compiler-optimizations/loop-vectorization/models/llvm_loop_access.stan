// Loops from LLVM's LoopAccessAnalysis tests,
// llvm/test/Analysis/LoopAccessAnalysis/{forward-loop-carried,
// forward-loop-independent,loops-with-indirect-reads-and-writes,
// store-to-invariant-check1,safe-with-dep-distance}.ll, translated to
// 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
data {
  int<lower=1> N;
  int<lower=1> J;
  vector[N] b;
  vector[J] w;
  array[N] int<lower=1, upper=N> idx;
}
parameters {
  real mu;
}
model {
  vector[N + 8] a;
  vector[N] c;

  // LLVM LoopAccessAnalysis/forward-loop-carried.ll
  // C: for (i = 0; i < N; i++) { A[i+8] = B[i] + 2; C[i] = A[i] * 2; }
  // LLVM: Forward dependence, "Memory dependences are safe".
  // Design: true dependence S1->S2 {Lt, 8}.
  // Emitted: vec a[9:(N+8)] = b + 2; vec c[1:N] = a[1:N] * 2.
  for (n in 1 : N) {
    a[n + 8] = b[n] + 2;
    c[n] = a[n] * 2;
  }

  // LLVM LoopAccessAnalysis/forward-loop-independent.ll
  // C: for (i = 0; i < N; i++) { A[i+1] = B[i] + 1; A[i] = B[i] + 2; C[i] = A[i] * 2; }
  // LLVM: Forward x3, safe.
  // Design: S1->S2 {Lt, 1} output, S2->S3 {Eq}, S1->S3 {Lt, 1}.
  // Emitted: three vec statements, in order.
  for (n in 1 : N) {
    a[n + 1] = b[n] + 1;
    a[n] = b[n] + 2;
    c[n] = a[n] * 2;
  }

  // LLVM LoopAccessAnalysis/loops-with-indirect-reads-and-writes.ll
  // C: for (i = 0; i < N; i++) { A[idx[i]] = B[i]; C[i] = A[idx[i]]; }
  // LLVM: IndirectUnsafe.
  // Design: Varying subscript: confused.
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    a[idx[n]] = b[n];
    c[n] = a[idx[n]];
  }

  // LLVM LoopAccessAnalysis/store-to-invariant-check1.ll (inner loop over j)
  // C: for (i) for (j = 0; j < M; j++) v[i] = w[j] + v[i];
  // LLVM: "Non vectorizable stores to invariant address".
  // Design: Invariant vs Invariant, same element every iteration: self
  // confused.
  // Emitted: seq (loop unchanged).
  {
    vector[N] v;
    for (i in 1 : N) {
      for (j in 1 : J) {
        v[i] = w[j] + v[i];
      }
    }
    target += sum(v);
  }

  // LLVM LoopAccessAnalysis/safe-with-dep-distance.ll
  // C: for (i = 0; i < N; i++) A[i+4] = A[i] * 2;
  // LLVM: safe up to vector width 4.
  // Design: true self dependence {Lt, 4}; a whole-vector statement is
  // illegal.
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    a[n + 4] = a[n] * 2;
  }

  target += sum(a) + sum(c);
  mu ~ normal(0, 1);
}
