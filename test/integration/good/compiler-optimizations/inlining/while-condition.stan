functions {
  int inc(int n) { return n + 1; }
}
data { int N; }
parameters { real theta; }
model {
  int k = 0;
  while (inc(k * 2) < N) {
    k += 1;
    target += -square(theta - k);
  }
}
