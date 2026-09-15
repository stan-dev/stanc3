// Loops from the GCC loop-distribution test suite,
// gcc/testsuite/gcc.dg/tree-ssa/ldist-1.c .. ldist-4.c, translated to
// 1-based Stan. GCC's ldist pass has a cache-reuse cost model and declines
// to split several legal cases; agreement with this design is expected on
// legality, not on policy.
// See design-docs/active/vectorize-loop-fission.md section 7.8.2.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example.
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
  // 1. GCC gcc.dg/tree-ssa/ldist-1.c
  // C: for (i = 0; i < N; i++) { mya[i] = ia[i]*oxa[i] + ib[i]*oxb[i];
  //      myb[i] = -ia[i]*oxb[i] + ib[i]*oxa[i];
  //      oya[i] = mya[i] >> 10; oyb[i] = myb[i] >> 10; }
  // GCC: legal to distribute, not split (cost model).
  // Design: {Eq} edges only.
  // Emitted: four vec statements.
  {
    vector[N] mya1;
    vector[N] myb1;
    vector[N] oya1;
    vector[N] oyb1;
    for (n in 1 : N) {
      mya1[n] = ia[n] .* oxa[n] + ib[n] .* oxb[n];
      myb1[n] = -ia[n] .* oxb[n] + ib[n] .* oxa[n];
      oya1[n] = mya1[n] / 1024;
      oyb1[n] = myb1[n] / 1024;
    }
    target += sum(mya1) + sum(myb1) + sum(oya1) + sum(oyb1);
  }

  // 2. GCC gcc.dg/tree-ssa/ldist-2.c
  // C: for (i = 1; i < N; i++) { a[i] += c[i]; b[i] = a[i-1] + 1; }
  // GCC: legal to distribute, not split (cost model).
  // Design: true dependence S1->S2 {Lt, 1}.
  // Emitted: vec a2[2:N] = a2[2:N] + c[2:N]; vec b2[2:N] = a2[1:(N-1)] + 1.
  {
    vector[N] a2;
    vector[N] b2;
    for (n in 2 : N) {
      a2[n] = a2[n] + c[n];
      b2[n] = a2[n - 1] + 1;
    }
    target += sum(a2) + sum(b2);
  }

  // 3. GCC gcc.dg/tree-ssa/ldist-3.c
  // C: for (i = 2; i < N-1; i++) { a[i] = k * i; b[i] = a[i-2] + k;
  //      c[i] = b[i] + a[i+1]; d[i] = c[i-1] + k + i; }
  // GCC: not split (statements stay fused).
  // Design: S1->S2 {Lt, 2}, S2->S3 {Eq}, anti S3->S1 {Lt, 1}: cycle
  // {S1, S2, S3}; S3->S4 {Lt, 1}.
  // Emitted: S1-S3 seq (cycle); S4 seq (uses n as a value); blocks fuse:
  // loop unchanged.
  {
    vector[N + 1] a3;
    vector[N] b3;
    vector[N] cc3;
    vector[N] d3;
    for (n in 3 : N) {
      a3[n] = k * n;
      b3[n] = a3[n - 2] + k;
      cc3[n] = b3[n] + a3[n + 1];
      d3[n] = cc3[n - 1] + k + n;
    }
    target += sum(a3) + sum(b3) + sum(cc3) + sum(d3);
  }

  // 4. GCC gcc.dg/tree-ssa/ldist-4.c
  // C: for (i = 0; i < N; i++) for (j = 1; j < M; j++)
  //      { a[j] = k * i; b[i][j] = a[j-1] + k; }
  // GCC: not distributed.
  // Design (inner loop over j): true dependence S1->S2 {Lt, 1}.
  // Emitted: S1 seq (invariant right-hand side), then
  // vec b4[i, 2:J] = a4[1:(J-1)] + k.
  {
    vector[J] a4;
    matrix[N, J] b4;
    for (i in 1 : N) {
      for (j in 2 : J) {
        a4[j] = k * i;
        b4[i, j] = a4[j - 1] + k;
      }
    }
    target += sum(a4) + sum(b4);
  }

  mu ~ normal(0, 1);
}
