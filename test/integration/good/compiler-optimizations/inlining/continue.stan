functions {
  int inc(int n) {
    return n + 1;
  }
}
data {
  int N;
}
parameters {
  real theta;
}
model {
  int k = 0;
  while (inc(k * 2) < N) {
    k += 1;
    if (k > 3) {
      continue;
    }
    target += -square(theta - k);
  }
  int m = N;
  for (i in 1:inc(m * 1)) {
    if (i == 2) {
      m = 1;
      continue;
    }
    target += -square(theta - i);
  }
}
