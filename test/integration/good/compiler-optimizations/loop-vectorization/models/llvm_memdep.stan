// Loops from LLVM's loop vectorizer test
// llvm/test/Transforms/LoopVectorize/memdep.ll, translated to 1-based Stan.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
data {
  int<lower=1> N;
}
parameters {
  real mu;
}
model {
  vector[N + 2] a;
  vector[N] b;
  real prev = 0;

  // LLVM LoopVectorize/memdep.ll f1_vec
  // C: for (i = 0; i < N; i++) A[i] = A[i+1] + 1;
  // LLVM: vectorized ("no plausible dependence").
  // Design: anti self dependence, read of a later iteration: no edge.
  // Emitted: vec a[1:N] = a[2:(N+1)] + 1.
  for (n in 1 : N) {
    a[n] = a[n + 1] + 1;
  }

  // LLVM LoopVectorize/memdep.ll f2_novec
  // C: for (i = 0; i < N; i++) A[i+1] = A[i] + 1;
  // LLVM: not vectorized (dependence distance 1).
  // Design: true self dependence {Lt, 1}: recurrence.
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    a[n + 1] = a[n] + 1;
  }

  // LLVM LoopVectorize/memdep.ll f3
  // C: for (i = 0; i < N; i++) A[i+2] = A[i] + 1;
  // LLVM: vectorized at width 2 only.
  // Design: true self dependence {Lt, 2}; a whole-vector statement is
  // illegal (agrees with "not at width >= 3").
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    a[n + 2] = a[n] + 1;
  }

  // LLVM LoopVectorize/memdep.ll f6
  // C: for (i = 0; i < N; i++) { B[i] = tmp; tmp = A[i]; }
  // LLVM: not vectorized ("dependence through a phi").
  // Design: prev has subs = []: confused in both directions.
  // Emitted: seq (loop unchanged).
  for (n in 1 : N) {
    b[n] = prev;
    prev = a[n];
  }

  target += sum(a) + sum(b) + prev;
  mu ~ normal(0, 1);
}
