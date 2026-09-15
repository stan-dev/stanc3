// TSVC "Control Flow" loops, translated 1-based.
// Source: UoB-HPC/TSVC_2 src/tsvc.c (Callahan, Dongarra and Levine 1988;
// Maleki et al. 2011). `IfElse` statements are single sequential leaves of
// the loop dependence graph (no if-conversion, roadmap item 6); effectful
// statements (a void call, `reject`) stay sequential but do not stop
// independent neighbours from being hoisted; `break` bails the whole loop.
// Each example sits in its own block, and every local it writes carries the
// example number as a suffix (a3 belongs to example 3) so the generated MIR
// and C++ can be matched to the example. Examples are ordered by outcome:
// partially vectorized loops first, then loops the pass leaves unchanged.
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
  // ---- Partially vectorized: some statements hoist, others stay in a loop ----

  // 1. TSVC s471 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: x[i] = b[i] + d[i] * d[i]; s471s(); b[i] = c[i] + d[i] * e[i];
  // intent: call statement in the loop
  // graph: S2 effectful; S1, S3 isolated
  // emitted: S1 vec, S2 seq, S3 vec (S1 before S3 by position; `foo` cannot
  //          touch `x1`/`b1` in Stan's value semantics)
  {
    vector[N] b1 = b0;
    vector[N] x1;
    for (n in 1 : N) {
      x1[n] = b1[n] + d[n] * d[n];
      foo();
      b1[n] = c[n] + d[n] * e[n];
    }
    target += sum(b1) + sum(x1);
  }

  // 2. TSVC s481 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (d[i] < 0.) exit(0); a[i] += b[i] * c[i];
  // intent: non-local goto (exit)
  // graph: S1 `IfElse` leaf with effect; S2 isolated   emitted: S1 seq, S2 vec
  {
    vector[N] a2 = a0;
    for (n in 1 : N) {
      if (d[n] < 0) reject("neg");
      a2[n] = a2[n] + b0[n] .* c[n];
    }
    target += sum(a2);
  }

  // ---- Left unchanged: no statement can be hoisted ----

  // 3. TSVC s271 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (b[i] > 0.) a[i] += b[i] * c[i];
  // intent: loop with singularity handling; needs if-conversion
  // graph: `IfElse` leaf   emitted: seq (roadmap item 6)
  {
    vector[N] a3 = a0;
    for (n in 1 : N) {
      if (b0[n] > 0) a3[n] = a3[n] + b0[n] * c[n];
    }
    target += sum(a3);
  }

  // 4. TSVC s441 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: if (d[i] < 0.) a[i] += b[i]*c[i]; else if (d[i] == 0.) a[i] += b[i]*b[i]; else a[i] += c[i]*c[i];
  // intent: arithmetic if
  // graph: `IfElse` leaf   emitted: seq
  {
    vector[N] a4 = a0;
    for (n in 1 : N) {
      if (d[n] < 0) a4[n] = a4[n] + b0[n] * c[n];
      else if (d[n] == 0) a4[n] = a4[n] + b0[n] * b0[n];
      else a4[n] = a4[n] + c[n] * c[n];
    }
    target += sum(a4);
  }

  // 5. TSVC s452 (Control Flow / induction), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] = b[i] + c[i] * (real_t) (i+1);
  // intent: induction variable used as a value
  // graph: no edge   emitted: seq (existing bail "loop variable used as a value")
  {
    vector[N] a5 = a0;
    for (n in 1 : N) {
      a5[n] = b0[n] + c[n] * (n + 1);
    }
    target += sum(a5);
  }

  // 6. TSVC s482 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
  // C: a[i] += b[i] * c[i]; if (c[i] > b[i]) break;
  // intent: non-local goto (break)
  // graph: `breaks`   emitted: bail
  {
    vector[N] a6 = a0;
    for (n in 1 : N) {
      a6[n] = a6[n] + b0[n] .* c[n];
      if (c[n] > b0[n]) break;
    }
    target += sum(a6);
  }
}
