// The first three statements form a dependence cycle through a and b, and
// the fourth uses the loop variable n as a value. No statement can become a
// vector statement. Translated to 1-based Stan.
// Source: GCC gcc.dg/tree-ssa/ldist-3.c
// C: for (i = 2; i < N-1; i++) { a[i] = k * i; b[i] = a[i-2] + k;
//      c[i] = b[i] + a[i+1]; d[i] = c[i-1] + k + i; }
// Dependences: S1->S2 {Lt, 2}, S2->S3 {Eq}, anti S3->S1 {Lt, 1}: cycle
//   {S1, S2, S3}; S3->S4 {Lt, 1}
// Desired outcome: left as a loop.
// S1-S3 stay in the loop because of the cycle and S4 because it uses n as a
// value, so the remaining loops fuse back into the original loop. GCC also
// keeps the statements in one loop.
data {
  int<lower=1> N;
  int k;
}
model {
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
