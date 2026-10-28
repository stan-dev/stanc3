// TSVC "Symbolics" loop (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011), from UoB-HPC/TSVC_2 src/tsvc.c. The loop is
// translated to 1-based Stan with the loop variable `n`; array sizes are
// chosen so every subscript is in range. Written variables are model-block
// locals initialised from data.
// Outcome on feat/loop-vectorization-dependence: Left unchanged
// 6. TSVC s431 (Symbolics), UoB-HPC/TSVC_2 src/tsvc.c
// C: int k = 2*inc_1d - 1; for (int i = 0; i < LEN_1D; i++) a[i] = a[i+k] + b[i];
// Intent: needs the value of k to decide.
// Design: a6[n] vs a6[n + k]: the offsets differ by the symbol k, so the
// pair is confused -> seq (correct). Contrast design example 12, where
// both sides carry the same k and the symbols cancel.
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
