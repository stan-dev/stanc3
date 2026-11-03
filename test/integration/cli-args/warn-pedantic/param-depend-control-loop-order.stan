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
    c[n] = b[n + 1];              // a later iteration writes b[n + 1]
    d[n] = b[n];                  // b[n] = a runs after this read
    b[n] = a;
    if (c[n] > 0) target += 1;    // no warning, because c[n] does not depend on a
    if (d[n] > 0) target += 1;    // no warning, because d[n] does not depend on a
    if (b[n] > 0) target += 1;    // warning, because b[n] = a
  }
}
