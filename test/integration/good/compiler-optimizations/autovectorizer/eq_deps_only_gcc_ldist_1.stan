// Four statements where the only dependences are loop independent ({Eq}),
// so every statement becomes a vector statement. Translated to 1-based Stan.
// Source: GCC gcc.dg/tree-ssa/ldist-1.c
// C: for (i = 0; i < N; i++) { mya[i] = ia[i]*oxa[i] + ib[i]*oxb[i];
//      myb[i] = -ia[i]*oxb[i] + ib[i]*oxa[i];
//      oya[i] = mya[i] >> 10; oyb[i] = myb[i] >> 10; }
// Dependences: {Eq} edges only
// Desired outcome: fully vectorized, four vector statements.
// GCC finds distribution legal here but does not split the loop because of
// its cache-reuse cost model.
data {
  int<lower=1> N;
  vector[N] ia;
  vector[N] ib;
  vector[N] oxa;
  vector[N] oxb;
}
model {
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
