functions {
  real bar(array[] real y_slice, int start, int end) {
    if (size(y_slice) > 1) {
      return reduce_sum(bar, y_slice, 1);
    } else {
      return normal_lpdf(y_slice | 0, 1);
    }
  }
}
data {
  int N;
}
transformed data {
  print(N);

  int p;
  int q;
  int p2 = q + 23;
}
parameters {
  array[N] real y1;
}
transformed parameters {
  jacobian += p * p2;

  real h;
}
model {
  real theta;
  real zero;
  // should warn for both theta and zero
  theta ~ normal(zero, 1);
  zero = 0;

  // warning is not transitive
  int x = p;
  print(x);

  real g;

  // this should warn for g and h, not 'target'
  target += normal_lpdf(g | h, 1);

  // this shouldn't warn because the above does.
  print(g);

  // should have no warnings
  target += reduce_sum(bar, y1, 1);
}
