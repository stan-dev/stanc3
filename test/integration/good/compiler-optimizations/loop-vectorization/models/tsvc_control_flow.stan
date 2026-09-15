// TSVC "Control Flow" loops, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `IfElse` statements are single sequential leaves of
// the loop dependence graph (no if-conversion, roadmap item 6); effectful
// statements (a void call, `reject`) stay sequential but do not stop
// independent neighbours from being hoisted; `break` bails the whole loop.
functions {
  void foo() {
    print("foo");
  }
}
data {
  int<lower=1> N;
  vector[N] a0;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N] a = a0;
  vector[N] b = b0;
  vector[N] x;

  // TSVC s271 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (b[i] > 0.) a[i] += b[i] * c[i];
  // intent: loop with singularity handling; needs if-conversion
  // graph: `IfElse` leaf   emitted: seq (roadmap item 6)
  for (n in 1 : N) {
    if (b[n] > 0) a[n] = a[n] + b[n] * c[n];
  }

  // TSVC s441 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (d[i] < 0.) a[i] += b[i]*c[i]; else if (d[i] == 0.) a[i] += b[i]*b[i]; else a[i] += c[i]*c[i];
  // intent: arithmetic if
  // graph: `IfElse` leaf   emitted: seq
  for (n in 1 : N) {
    if (d[n] < 0) a[n] = a[n] + b[n] * c[n];
    else if (d[n] == 0) a[n] = a[n] + b[n] * b[n];
    else a[n] = a[n] + c[n] * c[n];
  }

  // TSVC s452 (Control Flow / induction), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = b[i] + c[i] * (real_t) (i+1);
  // intent: induction variable used as a value
  // graph: no edge   emitted: seq (existing bail "loop variable used as a value")
  for (n in 1 : N) {
    a[n] = b[n] + c[n] * (n + 1);
  }

  // TSVC s471 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: x[i] = b[i] + d[i] * d[i]; s471s(); b[i] = c[i] + d[i] * e[i];
  // intent: call statement in the loop
  // graph: S2 effectful; S1, S3 isolated
  // emitted: S1 vec, S2 seq, S3 vec (S1 before S3 by position; `foo` cannot
  //          touch `x`/`b` in Stan's value semantics)
  for (n in 1 : N) {
    x[n] = b[n] + d[n] * d[n];
    foo();
    b[n] = c[n] + d[n] * e[n];
  }

  // TSVC s481 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (d[i] < 0.) exit(0); a[i] += b[i] * c[i];
  // intent: non-local goto (exit)
  // graph: S1 `IfElse` leaf with effect; S2 isolated   emitted: S1 seq, S2 vec
  for (n in 1 : N) {
    if (d[n] < 0) reject("neg");
    a[n] = a[n] + b[n] .* c[n];
  }

  // TSVC s482 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] += b[i] * c[i]; if (c[i] > b[i]) break;
  // intent: non-local goto (break)
  // graph: `breaks`   emitted: bail
  for (n in 1 : N) {
    a[n] = a[n] + b[n] .* c[n];
    if (c[n] > b[n]) break;
  }

  target += sum(a);
  target += sum(b);
  target += sum(x);
}
