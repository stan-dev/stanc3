functions {
  real g(real[] y_slice, int start, int end) {
    if(size(y_slice) > 1) {
      return reduce_sum(g, y_slice, 1);
    } else {
      return normal_lpdf(y_slice | 0, 1);
    }
  }
  real h(real[] y_slice, int start, int end, real[] a) {
    if(size(a) > 1) {
      return reduce_sum(h, y_slice, 1, a[start:end]);
    } else {
      return normal_lpdf(a | 0, 1);
    }
  }
  real foo_lpdf(real[] y_slice, int start, int end) {
    return normal_lpdf(y_slice| 0, 1);    
  }
}

transformed data {
  int N = 100;
}

parameters {
  real y1[N];
  real y2[N];
  real y3[N];
}

model {
  target += reduce_sum(g, y1, 1);
  target += reduce_sum(h, y2, 1, y2);
  target += reduce_sum(foo_lpdf, y3, 1);
}