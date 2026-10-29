// S1 writes `a5[n]` and S2 sums `a5[n]` and `a5[n + 1]`, the element that
// the next iteration's S1 overwrites. The true and anti dependences form a
// cycle, so the loop stays as written.
// Source: TSVC s1244 (NodeSplitting), UoB-HPC/TSVC_2 src/tsvc.c
// C: for (int i = 0; i < LEN_1D-1; i++) { a[i] = b[i] + c[i] * c[i] + b[i]*b[i] + c[i]; d[i] = a[i] + a[i+1]; }
// Intent: cycle with true and anti dependency.
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
  vector[N + 1] a5 = a0;
  vector[N] b5 = b0;
  vector[N] d5 = d0;
  for (n in 1 : N) {
    a5[n] = b5[n] + c[n] .* c[n];
    d5[n] = a5[n] + a5[n + 1];
  }
  target += sum(a5);
  target += sum(d5);
}
