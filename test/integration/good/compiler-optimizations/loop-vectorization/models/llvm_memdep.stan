// Loops from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example.
data {
  int<lower=1> N;
}
parameters {
  real mu;
}
model {
  // 1. LLVM LoopVectorize/memdep.ll f1_vec
  // C: for (i = 0; i < N; i++) A[i] = A[i+1] + 1;
  // LLVM: vectorized ("no plausible dependence").
  // Design: anti self dependence, read of a later iteration: no edge.
  // Emitted: vec a1[1:N] = a1[2:(N+1)] + 1.
  {
    vector[N + 1] a1;
    for (n in 1 : N) {
      a1[n] = a1[n + 1] + 1;
    }
    target += sum(a1);
  }

  // 2. LLVM LoopVectorize/memdep.ll f2_novec
  // C: for (i = 0; i < N; i++) A[i+1] = A[i] + 1;
  // LLVM: not vectorized (dependence distance 1).
  // Design: true self dependence {Lt, 1}: recurrence.
  // Emitted: seq (loop unchanged).
  {
    vector[N + 1] a2;
    for (n in 1 : N) {
      a2[n + 1] = a2[n] + 1;
    }
    target += sum(a2);
  }

  // 3. LLVM LoopVectorize/memdep.ll f3
  // C: for (i = 0; i < N; i++) A[i+2] = A[i] + 1;
  // LLVM: vectorized at width 2 only.
  // Design: true self dependence {Lt, 2}; a whole-vector statement is
  // illegal (agrees with "not at width >= 3").
  // Emitted: seq (loop unchanged).
  {
    vector[N + 2] a3;
    for (n in 1 : N) {
      a3[n + 2] = a3[n] + 1;
    }
    target += sum(a3);
  }

  // 4. LLVM LoopVectorize/memdep.ll f6
  // C: for (i = 0; i < N; i++) { B[i] = tmp; tmp = A[i]; }
  // LLVM: not vectorized ("dependence through a phi").
  // Design: prev4 has subs = []: confused in both directions.
  // Emitted: seq (loop unchanged).
  {
    vector[N] a4;
    vector[N] b4;
    real prev4 = 0;
    for (n in 1 : N) {
      b4[n] = prev4;
      prev4 = a4[n];
    }
    target += sum(b4) + prev4;
  }

  mu ~ normal(0, 1);
}
