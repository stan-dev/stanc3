// TSVC "Expansion" loops (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). Every loop here carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (s3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  // 1. TSVC s251 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s = b[i] + c[i] * d[i]; a[i] = s * s;
  // intent: scalar expansion; vectorizable after expansion
  // graph: `s1` confused (subs = [])   emitted: seq (roadmap item 2)
  {
    vector[N] a1 = a0;
    for (n in 1 : N) {
      real s1 = b[n] + c[n] * d[n];
      a1[n] = s1 * s1;
    }
    target += sum(a1);
  }

  // 2. TSVC s252 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s = b[i] * c[i]; a[i] = s + t; t = s;
  // intent: loop with ambiguous scalar temporary
  // graph: confused   emitted: seq
  {
    vector[N] a2 = a0;
    real t2 = 0;
    for (n in 1 : N) {
      real s2 = b[n] * c[n];
      a2[n] = s2 + t2;
      t2 = s2;
    }
    target += sum(a2) + t2;
  }

  // 3. TSVC s254 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = (b[i] + x) * 0.5; x = b[i];
  // intent: carry-around variable
  // graph: confused   emitted: seq
  {
    vector[N] a3 = a0;
    real x3 = 0;
    for (n in 1 : N) {
      a3[n] = (b[n] + x3) * 0.5;
      x3 = b[n];
    }
    target += sum(a3) + x3;
  }

  // 4. TSVC s258 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (a[i] > 0.) s = d[i] * d[i]; b[i] = s * c[i] + d[i];
  // intent: wrap-around scalar under an if
  // graph: `IfElse` leaf + `s4` confused   emitted: seq
  {
    real s4 = 0;
    vector[N] b4 = b;
    for (n in 1 : N) {
      if (a0[n] > 0) s4 = d[n] * d[n];
      b4[n] = s4 * c[n] + d[n];
    }
    target += sum(b4);
  }

  // 5. TSVC s261 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: t = a[i] + b[i]; a[i] = t + c[i-1]; t = c[i] * d[i]; c[i] = t;
  // intent: scalar renaming
  // graph: confused   emitted: seq
  {
    vector[N] a5 = a0;
    vector[N] c5 = c;
    real t5 = 0;
    for (n in 2 : N) {
      t5 = a5[n] + b[n];
      a5[n] = t5 + c5[n - 1];
      t5 = c5[n] * d[n];
      c5[n] = t5;
    }
    target += sum(a5) + sum(c5);
  }

  // 6. TSVC s453 (Expansion / induction variable), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s += 2.; a[i] = s * b[i];
  // intent: induction variable recognition
  // graph: confused   emitted: seq
  {
    vector[N] a6 = a0;
    real s6 = 0;
    for (n in 1 : N) {
      s6 += 2;
      a6[n] = s6 * b[n];
    }
    target += sum(a6);
  }
}
