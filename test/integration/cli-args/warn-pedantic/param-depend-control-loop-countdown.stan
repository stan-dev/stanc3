// This test runs the loop of param-depend-control-loop-order.stan from N - 1
// down to 2. A Stan loop only counts up, so the index is N - m.

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
    c[N - m] = b[N - m - 1];          // a later iteration writes b[N - m - 1]
    d[N - m] = b[N - m];              // b[N - m] = a runs after this read
    b[N - m] = a;
    if (c[N - m] > 0) target += 1;    // no warning, because c[N - m] does not depend on a
    if (d[N - m] > 0) target += 1;    // no warning, because d[N - m] does not depend on a
    if (b[N - m] > 0) target += 1;    // warning, because b[N - m] = a
  }
}
