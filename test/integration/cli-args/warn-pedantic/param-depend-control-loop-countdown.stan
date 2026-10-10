// Stan loops only count up, so a loop that walks a vector downwards goes
// through an index assigned in the loop or an index that is not the loop
// variable plus a constant. Neither index is compared, so the write of
// b[n + 1] in the previous iteration is kept and both ifs depend on a.
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
  for (m in 1:(N - 1)) {
    int n = N - m;                // n counts down from N - 1 to 1
    c[n] = b[n + 1];              // b[n + 1] was written in the previous iteration
    b[n] = a;
    if (c[n] > 0) target += 1;    // warns: depends on a
  }
  for (m in 1:(N - 1)) {
    d[N - m] = b[N - m + 1];      // b[N - m + 1] was written in the previous iteration
    b[N - m] = a;
    if (d[N - m] > 0) target += 1;    // warns: depends on a
  }
}
