functions {
  real g1(array[] real y_slice, int start, int end) {
    return normal_lpdf(y_slice | 0, 1);
  }
  real g2(array[] vector y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      sum_lpdf += normal_lpdf(y_slice[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real g3(array[] row_vector y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      sum_lpdf += normal_lpdf(y_slice[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real g4(array[] matrix y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      sum_lpdf += normal_lpdf(to_vector(y_slice[n]) | 0, 1);
    }
    return sum_lpdf;
  }
  real g5(array[,] real y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      for (m in 1 : size(y_slice[n])) {
        sum_lpdf += normal_lpdf(y_slice[n, m] | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g6(array[,] vector y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      for (m in 1 : size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g7(array[,] row_vector y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      for (m in 1 : size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g8(array[,] matrix y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for (n in 1 : size(y_slice)) {
      for (m in 1 : size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h1(array[] real y, int start, int end, array[] real a) {
    return normal_lpdf(a[start : end] | 0, 1);
  }
  real h2(array[] real y, int start, int end, array[] vector a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      sum_lpdf += normal_lpdf(a[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real h3(array[] real y, int start, int end, array[] row_vector a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      sum_lpdf += normal_lpdf(a[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real h4(array[] real y, int start, int end, array[] matrix a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      sum_lpdf += normal_lpdf(to_vector(a[n]) | 0, 1);
    }
    return sum_lpdf;
  }
  real h5(array[] real y, int start, int end, array[,] real a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      for (m in 1 : size(a[n])) {
        sum_lpdf += normal_lpdf(a[n, m] | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h6(array[] real y, int start, int end, array[,] vector a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      for (m in 1 : size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h7(array[] real y, int start, int end, array[,] row_vector a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      for (m in 1 : size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h8(array[] real y, int start, int end, array[,] matrix a) {
    real sum_lpdf = 0.0;
    for (n in start : end) {
      for (m in 1 : size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
}
transformed data {
  int N = 2;
  array[N] real x;
}
parameters {
  array[N, N] matrix[N, N] a8;
  array[N, N] row_vector[N] a7;
  array[N, N] vector[N] a6;
  array[N, N] real a5;
  array[N] matrix[N, N] a4;
  array[N] row_vector[N] a3;
  array[N] vector[N] a2;
  array[N] real a1;
  
  array[N, N] matrix[N, N] y8;
  array[N, N] row_vector[N] y7;
  array[N, N] vector[N] y6;
  array[N, N] real y5;
  array[N] matrix[N, N] y4;
  array[N] row_vector[N] y3;
  array[N] vector[N] y2;
  array[N] real y1;
}
model {
  target += reduce_sum(h8, x, 1, a8);
  target += reduce_sum(h7, x, 1, a7);
  target += reduce_sum(h6, x, 1, a6);
  target += reduce_sum(h5, x, 1, a5);
  target += reduce_sum(h4, x, 1, a4);
  target += reduce_sum(h3, x, 1, a3);
  target += reduce_sum(h2, x, 1, a2);
  target += reduce_sum(h1, x, 1, a1);
  
  target += reduce_sum(g8, y8, 1);
  target += reduce_sum(g7, y7, 1);
  target += reduce_sum(g6, y6, 1);
  target += reduce_sum(g5, y5, 1);
  target += reduce_sum(g4, y4, 1);
  target += reduce_sum(g3, y3, 1);
  target += reduce_sum(g2, y2, 1);
  target += reduce_sum(g1, y1, 1);
}

