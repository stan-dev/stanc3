// TSVC "Indirect Addressing" loops, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). A gather through a read-only index array is a
// `Varying` subscript, but it can only create a dependence when the gathered
// variable is also written in the loop; here it never is, so every loop is a
// vector statement (multi-index gather/scatter).
data {
  int<lower=1> N;
  array[N] int<lower=1, upper=N> ip;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
  real s;
  vector[N] a_data;
}
parameters {
  real<lower=0> sigma;
}
model {
  vector[N] a = a0;

  // TSVC s4112 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] += b[ip[i]] * s;
  // intent: indirect addressing, gather; vectorizable
  // graph: `b` read-only: no edge   emitted: vec
  for (n in 1 : N) {
    a[n] = a[n] + b[ip[n]] * s;
  }

  // TSVC s4113 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[ip[i]] = b[ip[i]] + c[i];
  // intent: indirect addressing, scatter and gather; vectorizable
  // graph: `a2` written only, never read: no edge
  // emitted: vec `a2[ip] = b[ip] + c` (Stan multi-index assignment is
  //          in-order, so duplicate indices behave as in the loop)
  {
    vector[N] a2 = a0;
    for (n in 1 : N) {
      a2[ip[n]] = b[ip[n]] + c[n];
    }
    target += sum(a2);
  }

  // TSVC s4115 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
  // C: sum += a[i] * b[ip[i]];   (sparse dot product, written as a density)
  // intent: indirect addressing, reduction
  // graph: reduction: no edge   emitted: vec
  for (n in 1 : N) {
    target += normal_lpdf(a_data[n] | b[ip[n]], sigma);
  }

  // TSVC s491 (Indirect Addressing), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[ip[i]] = b[i] + c[i] * d[i];
  // intent: indirect addressing, scatter; vectorizable
  // graph: no edge   emitted: vec
  {
    vector[N] a3 = a0;
    for (n in 1 : N) {
      a3[ip[n]] = b[n] + c[n] .* d[n];
    }
    target += sum(a3);
  }

  target += sum(a);
}
