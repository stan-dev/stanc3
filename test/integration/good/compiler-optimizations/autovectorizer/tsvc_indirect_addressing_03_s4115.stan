// TSVC "Indirect Addressing" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). A gather through a read-only index array is a
// `Varying` subscript, but it can only create a dependence when the gathered
// variable is also written in the loop; here it never is, so the loop is a
// vector statement (multi-index gather).
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 3. TSVC s4115 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: sum += a[i] * b[ip[i]];   (sparse dot product, written as a density)
// intent: indirect addressing, reduction
// graph: reduction: no edge   emitted: vec
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] b;
  vector[N] a_data;
}
parameters {
  real<lower=0> sigma;
}
model {
  for (n in 1 : N) {
    target += normal_lpdf(a_data[n] | b[ip[n]], sigma);
  }
}
