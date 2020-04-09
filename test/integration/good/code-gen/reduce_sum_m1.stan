functions {
  real g(int start, int end, real[] y_slice) {
    if(size(y_slice) > 1) {
      return reduce_sum(g, y_slice, 1);
    } else {
      return normal_lpdf(y_slice | 0, 1);
    }
  }
}

transformed data {
  int N = 10;
}

parameters {
  real y[N]; 
}

model {
  target += reduce_sum(g, y, 1);
}
