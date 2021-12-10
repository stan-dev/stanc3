functions {
  real g(array[] real y_slice, int start, int end) {
    if (size(y_slice) > 1) {
      return reduce_sum(g, y_slice, 1);
    } else {
      return normal_lpdf(y_slice | 0, 1);
    }
  }
  real h(array[] real y_slice, int start, int end, array[] real a) {
    if (size(a) > 1) {
      return reduce_sum(h, y_slice, 1, a[start : end]);
    } else {
      return normal_lpdf(a | 0, 1);
    }
  }
  real foo_lpdf(array[] real y_slice, int start, int end) {
    return normal_lpdf(y_slice | 0, 1);
  }
}
transformed data {
  int N = 100;
}
parameters {
  array[N] real y1;
  array[N] real y2;
  array[N] real y3;
}
model {
  target += reduce_sum(g, y1, 1);
  target += reduce_sum(h, y2, 1, y2);
  target += reduce_sum(foo_lpdf, y3, 1);
  target += reduce_sum(foo_lupdf, y3, 1);
}

