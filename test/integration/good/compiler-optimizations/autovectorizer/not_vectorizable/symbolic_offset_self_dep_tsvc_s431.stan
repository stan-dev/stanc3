// `a6[n]` is written from `a6[n + k]` for a data integer `k`. The offsets
// differ by the symbol `k`. The pair is therefore confused and the loop stays
// as written.
// Source: TSVC s431 (Symbolics), UoB-HPC/TSVC_2 src/tsvc.c
// C: int k = 2*inc_1d - 1; for (int i = 0; i < LEN_1D; i++) a[i] = a[i+k] + b[i];
// Intent: needs the value of k to decide.
// Dependences: `a6[n]` vs `a6[n + k]`, confused
// Desired outcome: left as a loop.
// Contrast symbolic_offset_same_k.stan, where both sides carry the same `k`
// and the symbols cancel.
data {
  int<lower=2> N;
  int<lower=0> k;
  vector[N + k] a0k;
  vector[N] b;
}
model {
  vector[N + k] a6 = a0k;
  for (n in 1 : N) {
    a6[n] = a6[n + k] + b[n];
  }
  target += sum(a6);
}
