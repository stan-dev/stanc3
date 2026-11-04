// The reads of b[n + 1] and b[n] run before any write of those elements with a.
// Only the condition on b[n] should warn, and the prior count of a should be 1.
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
    // no iteration writes b[n + 1] before this read
    c[n] = b[n + 1];
    // b[n] = a runs after this read
    d[n] = b[n];
    b[n] = a;
    // no warning, because c[n] does not depend on a
    if (c[n] > 0) {
      target += 1;
    }
    // no warning, because d[n] does not depend on a
    if (d[n] > 0) {
      target += 1;
    }
    // warning, because b[n] = a
    if (b[n] > 0) {
      target += 1;
    }
  }
}
