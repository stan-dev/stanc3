functions {
  real twice(real x) {
    return x + x;
  }
  real g(real x) {
    return x * 3;
  }
}
data {
  real d;
}
parameters {
  real theta;
}
model {
  theta ~ normal(0, 1);
  target += twice(g(d) * 2);
}
