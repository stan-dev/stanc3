// TSVC "Indirect Addressing" loop, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). A gather through a read-only index array is a
// `Varying` subscript, but it can only create a dependence when the gathered
// variable is also written in the loop; here it never is, so the loop is a
// vector statement (multi-index gather/scatter).
// Outcome on feat/loop-vectorization-dependence: Fully vectorized
// 2. TSVC s4113 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
// C: a[ip[i]] = b[ip[i]] + c[i];
// intent: indirect addressing, scatter and gather; vectorizable
// graph: `a2` written only, never read: no edge
// emitted: vec `a2[ip] = b[ip] + c` (Stan multi-index assignment is
//          in-order, so duplicate indices behave as in the loop)
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
}
model {
  vector[N] a2 = a0;
  for (n in 1 : N) {
    a2[ip[n]] = b[ip[n]] + c[n];
  }
  target += sum(a2);
}
