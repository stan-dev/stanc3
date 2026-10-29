// Three statements alternate writes to `a3` and `b3`, and S3 reads
// `a3[n + 1]`, the element that the next iteration's S1 overwrites. The
// carried S3->S1 edge closes a cycle with the S1->S2->S3 chain, so the loop
// stays as written.
// Source: TSVC s243 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * d[i]; b[i] = a[i] + d[i] * e[i]; a[i] = b[i] + a[i+1] * d[i]; }
// Intent: false dependence cycle breaking.
// Dependences: S3 true/anti self and S3->S1 {Lt, 1}, S1->S2->S3 {Eq},
//   a cycle
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
  vector[N] e;
}
model {
  vector[N + 1] a3 = a0;
  vector[N] b3 = b0;
  vector[N] d3 = d0;
  for (n in 1 : N) {
    a3[n] = b3[n] + c[n] .* d3[n];
    b3[n] = a3[n] + d3[n] .* e[n];
    a3[n] = b3[n] + a3[n + 1] .* d3[n];
  }
  target += sum(a3);
  target += sum(b3);
}
