// The read `a5[j5]` is subscripted by a scalar `j5` that the body
// increments, so the subscript is `Varying` and the loop stays as written.
// Source: TSVC s121 (InductionVariable), UoB-HPC/TSVC_2 src/tsvc.c
// C: j = 1; for (int i = 0; i < LEN_1D-1; i++) { j++; a[i] = a[j] + b[i]; }
// Intent: induction variable ambiguity.
// Dependences: `j5` is written in the body, so `a5[j5]` is Varying
// Desired outcome: left as a loop.
data {
  int<lower=2> N;
  vector[N + 1] a0;
  vector[N] b;
}
model {
  vector[N + 1] a5 = a0;
  int j5 = 1;
  for (n in 1 : (N - 1)) {
    a5[n] = a5[j5] + b[n];
    j5 += 1;
  }
  target += sum(a5);
}
