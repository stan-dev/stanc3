// TSVC "Expansion" loops (scalar and array expansion), translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). Every loop here carries a dependence through a scalar
// written in the body, so the whole-variable access (subs = []) is confused
// and the loop must stay sequential until scalar expansion is implemented.
// Each loop sits in its own block because Stan forbids shadowing `s`.
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b;
  vector[N] c;
  vector[N] d;
}
model {
  vector[N] a = a0;
  real t = 0;
  real x = 0;

  // TSVC s251 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s = b[i] + c[i] * d[i]; a[i] = s * s;
  // intent: scalar expansion; vectorizable after expansion
  // graph: `s` confused (subs = [])   emitted: seq (roadmap item 2)
  {
    for (n in 1 : N) {
      real s = b[n] + c[n] * d[n];
      a[n] = s * s;
    }
  }

  // TSVC s252 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s = b[i] * c[i]; a[i] = s + t; t = s;
  // intent: loop with ambiguous scalar temporary
  // graph: confused   emitted: seq
  {
    for (n in 1 : N) {
      real s = b[n] * c[n];
      a[n] = s + t;
      t = s;
    }
  }

  // TSVC s254 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = (b[i] + x) * 0.5; x = b[i];
  // intent: carry-around variable
  // graph: confused   emitted: seq
  for (n in 1 : N) {
    a[n] = (b[n] + x) * 0.5;
    x = b[n];
  }

  // TSVC s258 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (a[i] > 0.) s = d[i] * d[i]; b[i] = s * c[i] + d[i];
  // intent: wrap-around scalar under an if
  // graph: `IfElse` leaf + `s` confused   emitted: seq
  {
    real s = 0;
    vector[N] b2 = b;
    for (n in 1 : N) {
      if (a[n] > 0) s = d[n] * d[n];
      b2[n] = s * c[n] + d[n];
    }
    target += sum(b2);
  }

  // TSVC s261 (Expansion), UoB-HPC/TSVC_2 src/tsvc.c
  // C: t = a[i] + b[i]; a[i] = t + c[i-1]; t = c[i] * d[i]; c[i] = t;
  // intent: scalar renaming
  // graph: confused   emitted: seq
  {
    vector[N] c2 = c;
    for (n in 2 : N) {
      t = a[n] + b[n];
      a[n] = t + c2[n - 1];
      t = c2[n] * d[n];
      c2[n] = t;
    }
    target += sum(c2);
  }

  // TSVC s453 (Expansion / induction variable), UoB-HPC/TSVC_2 src/tsvc.c
  // C: s += 2.; a[i] = s * b[i];
  // intent: induction variable recognition
  // graph: confused   emitted: seq
  {
    real s = 0;
    for (n in 1 : N) {
      s += 2;
      a[n] = s * b[n];
    }
  }

  target += sum(a);
}
