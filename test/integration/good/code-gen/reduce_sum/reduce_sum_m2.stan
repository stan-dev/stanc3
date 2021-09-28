functions {
  real g1(real[] y_slice, int start, int end) {
    return normal_lpdf(y_slice | 0, 1);
  }
  real g2(vector[] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      sum_lpdf += normal_lpdf(y_slice[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real g3(row_vector[] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      sum_lpdf += normal_lpdf(y_slice[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real g4(matrix[] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      sum_lpdf += normal_lpdf(to_vector(y_slice[n]) | 0, 1);
    }
    return sum_lpdf;
  }
  real g5(real[,] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      for(m in 1:size(y_slice[n])) {
        sum_lpdf += normal_lpdf(y_slice[n, m] | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g6(vector[,] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      for(m in 1:size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g7(row_vector[,] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      for(m in 1:size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real g8(matrix[,] y_slice, int start, int end) {
    real sum_lpdf = 0.0;
    for(n in 1:size(y_slice)) {
      for(m in 1:size(y_slice[n])) {
        sum_lpdf += normal_lpdf(to_vector(y_slice[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h1(real[] y, int start, int end, real[] a) {
    return normal_lpdf(a[start:end] | 0, 1);
  }
  real h2(real[] y, int start, int end, vector[] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      sum_lpdf += normal_lpdf(a[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real h3(real[] y, int start, int end, row_vector[] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      sum_lpdf += normal_lpdf(a[n] | 0, 1);
    }
    return sum_lpdf;
  }
  real h4(real[] y, int start, int end, matrix[] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      sum_lpdf += normal_lpdf(to_vector(a[n]) | 0, 1);
    }
    return sum_lpdf;
  }
  real h5(real[] y, int start, int end, real[,] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      for(m in 1:size(a[n])) {
        sum_lpdf += normal_lpdf(a[n, m] | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h6(real[] y, int start, int end, vector[,] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      for(m in 1:size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h7(real[] y, int start, int end, row_vector[,] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      for(m in 1:size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
  real h8(real[] y, int start, int end, matrix[,] a) {
    real sum_lpdf = 0.0;
    for(n in start:end) {
      for(m in 1:size(a[n])) {
        sum_lpdf += normal_lpdf(to_vector(a[n, m]) | 0, 1);
      }
    }
    return sum_lpdf;
  }
}

transformed data {
  int N = 2;
  real x[N];
}

parameters {
  matrix[N, N] a8[N, N];
  row_vector[N] a7[N, N]; 
  vector[N] a6[N, N];
  real a5[N, N];
  matrix[N, N] a4[N];
  row_vector[N] a3[N]; 
  vector[N] a2[N];
  real a1[N];
  
  matrix[N, N] y8[N, N];
  row_vector[N] y7[N, N]; 
  vector[N] y6[N, N];
  real y5[N, N];
  matrix[N, N] y4[N];
  row_vector[N] y3[N]; 
  vector[N] y2[N];
  real y1[N];
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
