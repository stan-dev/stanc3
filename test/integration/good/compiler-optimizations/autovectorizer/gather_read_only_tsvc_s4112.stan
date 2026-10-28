// TSVC "Indirect Addressing" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). A gather through a read-only index array is a
// `Varying` subscript, but it can only create a dependence when the gathered
// variable is also written in the loop; here it never is, so the loop is a
// vector statement (multi-index gather).
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 1. TSVC s4112 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[i] += b[ip[i]] * s;
// intent: indirect addressing, gather; vectorizable
// graph: `b` read-only: no edge   emitted: vec
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  real s;
}
model {
  vector[N] a1 = a0;
  for (n in 1 : N) {
    a1[n] = a1[n] + b[ip[n]] * s;
  }
  target += sum(a1);
}
