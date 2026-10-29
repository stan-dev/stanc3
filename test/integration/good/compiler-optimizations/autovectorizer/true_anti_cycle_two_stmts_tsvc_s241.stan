// S1 writes `a2[n]` and S2 reads `a2[n]` and `a2[n + 1]`, the element that
// the next iteration's S1 overwrites. The two dependences form a cycle, so
// without node splitting the loop correctly stays as written.
// Source: TSVC s241 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] * c[i] * d[i]; b[i] = a[i] * a[i+1] * d[i]; }
// Intent: needs preloading of a[i+1] (node splitting).
// Dependences: S1->S2 {Eq}, anti S2->S1 {Lt, 1}, a cycle
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b0;
  vector[N] d0;
  vector[N] c;
}
model {
  vector[N + 1] a2 = a0;
  vector[N] b2 = b0;
  vector[N] d2 = d0;
  for (n in 1 : N) {
    a2[n] = b2[n] .* c[n] .* d2[n];
    b2[n] = a2[n] .* a2[n + 1] .* d2[n];
  }
  target += sum(a2);
  target += sum(b2);
}
