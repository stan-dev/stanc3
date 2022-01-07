functions {
  real s(array[] real y_slice, int start, int end) {
    return reduce_sum(s, y_slice, 1);
  }
  real r() {
    int N;
    array[N] real y1d;
    real ts = reduce_sum(s, y1d, 1);
    return 0.0;
  }
}
data {
  int N;
  array[N] real y1d;
}
transformed data {
  real tsd = reduce_sum(s, y1d, 1);
}
