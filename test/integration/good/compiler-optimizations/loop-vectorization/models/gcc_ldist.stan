// Loops from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-1.c .. ldist-4.c, translated to
// 1-based Stan. GCC's ldist pass has a cache-reuse cost model and declines
// to split several legal cases; agreement with this design is expected on
// legality, not on policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
data {
  int<lower=1> N;
  int<lower=1> J;
  int k;
  vector[N] ia;
  vector[N] ib;
  vector[N] oxa;
  vector[N] oxb;
  vector[N] c;
}
parameters {
  real mu;
}
model {
  vector[N] mya;
  vector[N] myb;
  vector[N] oya;
  vector[N] oyb;

  // GCC gcc.dg/tree-ssa/ldist-1.c
  // C: for (i = 0; i < N; i++) { mya[i] = ia[i]*oxa[i] + ib[i]*oxb[i];
  //      myb[i] = -ia[i]*oxb[i] + ib[i]*oxa[i];
  //      oya[i] = mya[i] >> 10; oyb[i] = myb[i] >> 10; }
  // GCC: legal to distribute, not split (cost model).
  // Design: {Eq} edges only.
  // Emitted: four vec statements.
  for (n in 1 : N) {
    mya[n] = ia[n] .* oxa[n] + ib[n] .* oxb[n];
    myb[n] = -ia[n] .* oxb[n] + ib[n] .* oxa[n];
    oya[n] = mya[n] / 1024;
    oyb[n] = myb[n] / 1024;
  }
  target += sum(mya) + sum(myb) + sum(oya) + sum(oyb);

  // GCC gcc.dg/tree-ssa/ldist-2.c
  // C: for (i = 1; i < N; i++) { a[i] += c[i]; b[i] = a[i-1] + 1; }
  // GCC: legal to distribute, not split (cost model).
  // Design: true dependence S1->S2 {Lt, 1}.
  // Emitted: vec a[2:N] = a[2:N] + c[2:N]; vec b[2:N] = a[1:(N-1)] + 1.
  {
    vector[N] a;
    vector[N] b;
    for (n in 2 : N) {
      a[n] = a[n] + c[n];
      b[n] = a[n - 1] + 1;
    }
    target += sum(a) + sum(b);
  }

  // GCC gcc.dg/tree-ssa/ldist-3.c
  // C: for (i = 2; i < N-1; i++) { a[i] = k * i; b[i] = a[i-2] + k;
  //      c[i] = b[i] + a[i+1]; d[i] = c[i-1] + k + i; }
  // GCC: not split (statements stay fused).
  // Design: S1->S2 {Lt, 2}, S2->S3 {Eq}, anti S3->S1 {Lt, 1}: cycle
  // {S1, S2, S3}; S3->S4 {Lt, 1}.
  // Emitted: S1-S3 seq (cycle); S4 seq (uses n as a value); blocks fuse:
  // loop unchanged.
  {
    vector[N + 1] a;
    vector[N] b;
    vector[N] cc;
    vector[N] d;
    for (n in 3 : N) {
      a[n] = k * n;
      b[n] = a[n - 2] + k;
      cc[n] = b[n] + a[n + 1];
      d[n] = cc[n - 1] + k + n;
    }
    target += sum(a) + sum(b) + sum(cc) + sum(d);
  }

  // GCC gcc.dg/tree-ssa/ldist-4.c
  // C: for (i = 0; i < N; i++) for (j = 1; j < M; j++)
  //      { a[j] = k * i; b[i][j] = a[j-1] + k; }
  // GCC: not distributed.
  // Design (inner loop over j): true dependence S1->S2 {Lt, 1}.
  // Emitted: S1 seq (invariant right-hand side), then
  // vec b[i, 2:J] = a[1:(J-1)] + k.
  {
    vector[J] a;
    matrix[N, J] b;
    for (i in 1 : N) {
      for (j in 2 : J) {
        a[j] = k * i;
        b[i, j] = a[j - 1] + k;
      }
    }
    target += sum(a) + sum(b);
  }

  mu ~ normal(0, 1);
}
