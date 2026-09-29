data {
  int N;
}
parameters {
  real a;
}
model {
  vector[N] b;
  vector[N] c;
  vector[N] d;
  for (n in 1:(N - 1)) {
    c[n] = b[n + 1];              // b[n] below is written in a later iteration
    d[n] = b[n];                  // b[n] below is written later in this iteration
    b[n] = a;
    if (c[n] > 0) target += 1;    // no warning: never depends on a
    if (d[n] > 0) target += 1;    // no warning: never depends on a
    if (b[n] > 0) target += 1;    // warns: depends on a
  }
}
