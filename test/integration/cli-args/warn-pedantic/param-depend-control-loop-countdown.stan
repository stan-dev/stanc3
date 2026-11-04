// The loop of param-depend-control-loop-order.stan counting down with index N - m.
// Only the condition on b[N - m] should warn, and the prior count of a should be 1.
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
  for (m in 1:(N - 2)) {
    // no iteration writes b[N - m - 1] before this read
    c[N - m] = b[N - m - 1];
    // b[N - m] = a runs after this read
    d[N - m] = b[N - m];
    b[N - m] = a;
    // no warning, because c[N - m] does not depend on a
    if (c[N - m] > 0) {
      target += 1;
    }
    // no warning, because d[N - m] does not depend on a
    if (d[N - m] > 0) {
      target += 1;
    }
    // warning, because b[N - m] = a
    if (b[N - m] > 0) {
      target += 1;
    }
  }
}
