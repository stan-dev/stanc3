// S1 writes `a4[n]`, S2 updates `b4[n]`, and S3 writes `a4[n + 1]`, the
// element that the next iteration's S1 writes again. The output dependence
// S3->S1 closes a cycle with the S1->S2->S3 chain, so the loop stays as
// written.
// Source: TSVC s244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; ++i) { a[i] = b[i] + c[i] * d[i]; b[i] = c[i] + b[i]; a[i+1] = b[i] + a[i+1] * d[i]; }
// Intent: false dependence cycle breaking.
// Dependences: output S3->S1 {Lt, 1}, S1->S2 {Eq}, S2->S3 {Eq}, a cycle
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
}
model {
  vector[N + 1] a4 = a0;
  vector[N] b4 = b0;
  vector[N] d4 = d0;
  for (n in 1 : N) {
    a4[n] = b4[n] + c[n] .* d4[n];
    b4[n] = c[n] + b4[n];
    a4[n + 1] = b4[n] + a4[n + 1] .* d4[n];
  }
  target += sum(a4);
  target += sum(b4);
}
