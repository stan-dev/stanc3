// TSVC "Indirect Addressing" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). An access through a read-only index array is a
// `Varying` subscript, but it can only create a dependence when the indexed
// variable is also read in the loop; here it never is, so the loop is a
// vector statement (multi-index scatter).
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 4. TSVC s491 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[ip[i]] = b[i] + c[i] * d[i];
// intent: indirect addressing, scatter; vectorizable
// graph: no edge   emitted: vec
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a4 = a0;
  for (n in 1 : N) {
    a4[ip[n]] = b[n] + c[n] .* d[n];
  }
  target += sum(a4);
}
