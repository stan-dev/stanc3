// Loops from LLVM's LoopAccessAnalysis tests,
// llvm/test/Analysis/LoopAccessAnalysis/{forward-loop-carried,
// forward-loop-independent,loops-with-indirect-reads-and-writes,
// store-to-invariant-check1,safe-with-dep-distance}.ll, translated to
// 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example.
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
  // 1. LLVM LoopAccessAnalysis/forward-loop-carried.ll
  // C: for (i = 0; i < N; i++) { A[i+8] = B[i] + 2; C[i] = A[i] * 2; }
  // LLVM: Forward dependence, "Memory dependences are safe".
  // Design: true dependence S1->S2 {Lt, 8}.
  // Emitted: vec a1[9:(N+8)] = b + 2; vec c1[1:N] = a1[1:N] * 2.
  {
    vector[N + 8] a1;
    vector[N] c1;
    for (n in 1 : N) {
      a1[n + 8] = b[n] + 2;
      c1[n] = a1[n] * 2;
    }
    target += sum(a1) + sum(c1);
  }

  // 2. LLVM LoopAccessAnalysis/forward-loop-independent.ll
  // C: for (i = 0; i < N; i++) { A[i+1] = B[i] + 1; A[i] = B[i] + 2; C[i] = A[i] * 2; }
  // LLVM: Forward x3, safe.
  // Design: S1->S2 {Lt, 1} output, S2->S3 {Eq}, S1->S3 {Lt, 1}.
  // Emitted: three vec statements, in order.
  {
    vector[N + 1] a2;
    vector[N] c2;
    for (n in 1 : N) {
      a2[n + 1] = b[n] + 1;
      a2[n] = b[n] + 2;
      c2[n] = a2[n] * 2;
    }
    target += sum(a2) + sum(c2);
  }

  // 3. LLVM LoopAccessAnalysis/loops-with-indirect-reads-and-writes.ll
  // C: for (i = 0; i < N; i++) { A[idx[i]] = B[i]; C[i] = A[idx[i]]; }
  // LLVM: IndirectUnsafe.
  // Design: Varying subscript: confused.
  // Emitted: seq (loop unchanged).
  {
    vector[N] a3;
    vector[N] c3;
    for (n in 1 : N) {
      a3[idx[n]] = b[n];
      c3[n] = a3[idx[n]];
    }
    target += sum(a3) + sum(c3);
  }

  // 4. LLVM LoopAccessAnalysis/store-to-invariant-check1.ll (inner loop
  // over j)
  // C: for (i) for (j = 0; j < M; j++) v[i] = w[j] + v[i];
  // LLVM: "Non vectorizable stores to invariant address".
  // Design: Invariant vs Invariant, same element every iteration: self
  // confused.
  // Emitted: seq (loop unchanged).
  {
    vector[N] v4;
    for (i in 1 : N) {
      for (j in 1 : J) {
        v4[i] = w[j] + v4[i];
      }
    }
    target += sum(v4);
  }

  // 5. LLVM LoopAccessAnalysis/safe-with-dep-distance.ll
  // C: for (i = 0; i < N; i++) A[i+4] = A[i] * 2;
  // LLVM: safe up to vector width 4.
  // Design: true self dependence {Lt, 4}; a whole-vector statement is
  // illegal.
  // Emitted: seq (loop unchanged).
  {
    vector[N + 4] a5;
    for (n in 1 : N) {
      a5[n + 4] = a5[n] * 2;
    }
    target += sum(a5);
  }

  mu ~ normal(0, 1);
}
