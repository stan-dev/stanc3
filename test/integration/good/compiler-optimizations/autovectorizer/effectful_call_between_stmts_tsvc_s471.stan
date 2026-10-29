// A void call `foo()` sits between two independent assignments. The call is
// an effectful statement that stays in the loop, but it does not stop the
// independent statements around it from becoming vector statements.
// Source: TSVC s471 (Control Flow), UoB-HPC/TSVC_2 src/tsvc.c
// C: x[i] = b[i] + d[i] * d[i]; s471s(); b[i] = c[i] + d[i] * e[i];
// Intent: call statement in the loop
// Dependences: S2 effectful; S1, S3 isolated
// Desired outcome: partially vectorized. S1 and S3 become vector statements
//   (S1 before S3 by position) and S2 stays in a loop.
// `foo` cannot touch `x1` or `b1` under Stan's value semantics.
functions {
  void foo() {
    print("foo");
  }
}
data {
  int<lower=1> N;
  vector[N] b0;
  vector[N] c;
  vector[N] d;
  vector[N] e;
}
model {
  vector[N] b1 = b0;
  vector[N] x1;
  for (n in 1 : N) {
    x1[n] = b1[n] + d[n] * d[n];
    foo();
    b1[n] = c[n] + d[n] * e[n];
  }
  target += sum(b1) + sum(x1);
}
