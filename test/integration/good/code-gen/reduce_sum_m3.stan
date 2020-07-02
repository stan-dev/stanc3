functions {
  real f1(real[] y_slice, int start, int end) {
    return 0.0;
  }
  real f1a(real[] y_slice, int start, int end) {
    return 0.0;
  }
  real f2(vector[] y_slice, int start, int end) {
    return 0.0;
  }
  real f3(row_vector[] y_slice, int start, int end) {
    return 0.0;
  }
  real f4(matrix[] y_slice, int start, int end) {
    return 0.0;
  }
  real f5(real[,] y_slice, int start, int end) {
    return 0.0;
  }
  real f6(vector[,] y_slice, int start, int end) {
    return 0.0;
  }
  real f7(row_vector[,] y_slice, int start, int end) {
    return 0.0;
  }
  real f8(matrix[,] y_slice, int start, int end) {
    return 0.0;
  }
  real f9(int[] y_slice, int start, int end) {
    return 0.0;
  }
  real f10(int[,] y_slice, int start, int end) {
    return 0.0;
  }
  real f11(int[,,] y_slice, int start, int end) {
    return 0.0;
  }
  real f12(real[,,] y_slice, int start, int end) {
    return 0.0;
  }
  real g1(real[] y_slice, int start, int end, real a) {
    return 0.0;
  }
  real g2(real[] y_slice, int start, int end, vector a) {
    return 0.0;
  }
  real g3(real[] y_slice, int start, int end, row_vector a) {
    return 0.0;
  }
  real g4(real[] y_slice, int start, int end, matrix a) {
    return 0.0;
  }
  real g5(real[] y_slice, int start, int end, real[] a) {
    return 0.0;
  }
  real g6(real[] y_slice, int start, int end, vector[] a) {
    return 0.0;
  }
  real g7(real[] y_slice, int start, int end, row_vector[] a) {
    return 0.0;
  }
  real g8(real[] y_slice, int start, int end, matrix[] a) {
    return 0.0;
  }
  real g9(real[] y_slice, int start, int end, real[,] a) {
    return 0.0;
  }
  real g10(real[] y_slice, int start, int end, vector[,] a) {
    return 0.0;
  }
  real g11(real[] y_slice, int start, int end, row_vector[,] a) {
    return 0.0;
  }
  real g12(real[] y_slice, int start, int end, matrix[,] a) {
    return 0.0;
  }
  real s(real[] y_slice, int start, int end,
         int a, real b, vector c, row_vector d, matrix e,
         int[] f, real[] g, vector[] h, row_vector[] i, matrix[] j,
         int[,] k, real[,] l, vector[,] m, row_vector[,] n, matrix[,] o,
         int[,,] p, real[,,] q) {
    return reduce_sum(s, y_slice, 1,
                      a, b, c, d, e,
                      f, g, h, i, j,
                      k, l, m, n, o,
                      p, q);
  }
  real r() {
    int N;
    real y1d[N];
    vector[N] y2d[N];
    row_vector[N] y3d[N];
    matrix[N, N] y4d[N];
    real y5d[N, N];
    vector[N] y6d[N, N];
    row_vector[N] y7d[N, N];
    matrix[N, N] y8d[N, N];
    real y9d;
    vector[N] y10d;
    row_vector[N] y11d;
    matrix[N, N] y12d;
    int y13d;
    int y14d[N];
    int y15d[N, N];
    int y16d[N, N, N];
    real y17d[N, N, N];
    
    real y1[N];
    vector[N] y2[N];
    row_vector[N] y3[N];
    matrix[N, N] y4[N];
    real y5[N, N];
    vector[N] y6[N, N];
    row_vector[N] y7[N, N];
    matrix[N, N] y8[N, N];
    real y9;
    vector[N] y10;
    row_vector[N] y11;
    matrix[N, N] y12;
    real y17[N, N, N];
    
    real t1 = reduce_sum(f1, y1, 1);
    real t1a = reduce_sum(f1, y1, 1) + reduce_sum(f1a, y1, 1);
    real t2 = reduce_sum(f2, y2, 1);
    real t3 = reduce_sum(f3, y3, 1);
    real t4 = reduce_sum(f4, y4, 1);
    real t5 = reduce_sum(f5, y5, 1);
    real t6 = reduce_sum(f6, y6, 1);
    real t7 = reduce_sum(f7, y7, 1);
    real t8 = reduce_sum(f8, y8, 1);
    real t9 = reduce_sum(f9, y14d, 1);
    real t10 = reduce_sum(f10, y15d, 1);
    real t11 = reduce_sum(f11, y16d, 1);
    real t12 = reduce_sum(f12, y17, 1);
    real tg1 = reduce_sum(g1, y1, 1, y9);
    real tg2 = reduce_sum(g2, y1, 1, y10);
    real tg3 = reduce_sum(g3, y1, 1, y11);
    real tg4 = reduce_sum(g4, y1, 1, y12);
    real tg5 = reduce_sum(g5, y1, 1, y1);
    real tg6 = reduce_sum(g6, y1, 1, y2);
    real tg7 = reduce_sum(g7, y1, 1, y3);
    real tg8 = reduce_sum(g8, y1, 1, y4);
    real tg9 = reduce_sum(g9, y1, 1, y5);
    real tg10 = reduce_sum(g10, y1, 1, y6);
    real tg11 = reduce_sum(g11, y1, 1, y7);
    real tg12 = reduce_sum(g12, y1, 1, y8);
    real ts = reduce_sum(s, y1d, 1, y13d, y9, y10, y11, y12,
                         y14d, y1, y2, y3, y4,
                         y15d, y5, y6, y7, y8,
                         y16d, y17);
                         
    return 0.0;
  }
}

data {
  int N;
  real y1d[N];
  vector[N] y2d[N];
  row_vector[N] y3d[N];
  matrix[N, N] y4d[N];
  real y5d[N, N];
  vector[N] y6d[N, N];
  row_vector[N] y7d[N, N];
  matrix[N, N] y8d[N, N];
  real y9d;
  vector[N] y10d;
  row_vector[N] y11d;
  matrix[N, N] y12d;
  int y13d;
  int y14d[N];
  int y15d[N, N];
  int y16d[N, N, N];
  real y17d[N, N, N];
}

transformed data {
  real td1 = reduce_sum(f1, y1d, 1);
  real td1a = reduce_sum(f1, y1d, 1) + reduce_sum(f1a, y1d, 1);
  real td2 = reduce_sum(f2, y2d, 1);
  real td3 = reduce_sum(f3, y3d, 1);
  real td4 = reduce_sum(f4, y4d, 1);
  real td5 = reduce_sum(f5, y5d, 1);
  real td6 = reduce_sum(f6, y6d, 1);
  real td7 = reduce_sum(f7, y7d, 1);
  real td8 = reduce_sum(f8, y8d, 1);
  real td9 = reduce_sum(f9, y14d, 1);
  real td10 = reduce_sum(f10, y15d, 1);
  real td11 = reduce_sum(f11, y16d, 1);
  real td12 = reduce_sum(f12, y17d, 1);
  real tgd1 = reduce_sum(g1, y1d, 1, y9d);
  real tgd2 = reduce_sum(g2, y1d, 1, y10d);
  real tgd3 = reduce_sum(g3, y1d, 1, y11d);
  real tgd4 = reduce_sum(g4, y1d, 1, y12d);
  real tgd5 = reduce_sum(g5, y1d, 1, y1d);
  real tgd6 = reduce_sum(g6, y1d, 1, y2d);
  real tgd7 = reduce_sum(g7, y1d, 1, y3d);
  real tgd8 = reduce_sum(g8, y1d, 1, y4d);
  real tgd9 = reduce_sum(g9, y1d, 1, y5d);
  real tgd10 = reduce_sum(g10, y1d, 1, y6d);
  real tgd11 = reduce_sum(g11, y1d, 1, y7d);
  real tgd12 = reduce_sum(g12, y1d, 1, y8d);
  real tsd = reduce_sum(s, y1d, 1, y13d, y9d, y10d, y11d, y12d,
                        y14d, y1d, y2d, y3d, y4d,
                        y15d, y5d, y6d, y7d, y8d,
                        y16d, y17d);
}

parameters {
  real y1[N];
  vector[N] y2[N];
  row_vector[N] y3[N];
  matrix[N, N] y4[N];
  real y5[N, N];
  vector[N] y6[N, N];
  row_vector[N] y7[N, N];
  matrix[N, N] y8[N, N];
  real y9;
  vector[N] y10;
  row_vector[N] y11;
  matrix[N, N] y12;
  real y17[N, N, N];
}

model {
  real t1 = reduce_sum(f1, y1, 1);
  real t1a = reduce_sum(f1, y1, 1) + reduce_sum(f1a, y1, 1);
  real t2 = reduce_sum(f2, y2, 1);
  real t3 = reduce_sum(f3, y3, 1);
  real t4 = reduce_sum(f4, y4, 1);
  real t5 = reduce_sum(f5, y5, 1);
  real t6 = reduce_sum(f6, y6, 1);
  real t7 = reduce_sum(f7, y7, 1);
  real t8 = reduce_sum(f8, y8, 1);
  real t9 = reduce_sum(f9, y14d, 1);
  real t10 = reduce_sum(f10, y15d, 1);
  real t11 = reduce_sum(f11, y16d, 1);
  real t12 = reduce_sum(f12, y17, 1);
  real tg1 = reduce_sum(g1, y1, 1, y9);
  real tg2 = reduce_sum(g2, y1, 1, y10);
  real tg3 = reduce_sum(g3, y1, 1, y11);
  real tg4 = reduce_sum(g4, y1, 1, y12);
  real tg5 = reduce_sum(g5, y1, 1, y1);
  real tg6 = reduce_sum(g6, y1, 1, y2);
  real tg7 = reduce_sum(g7, y1, 1, y3);
  real tg8 = reduce_sum(g8, y1, 1, y4);
  real tg9 = reduce_sum(g9, y1, 1, y5);
  real tg10 = reduce_sum(g10, y1, 1, y6);
  real tg11 = reduce_sum(g11, y1, 1, y7);
  real tg12 = reduce_sum(g12, y1, 1, y8);
  real ts = reduce_sum(s, y1d, 1, y13d, y9, y10, y11, y12,
                       y14d, y1, y2, y3, y4,
                       y15d, y5, y6, y7, y8,
                       y16d, y17);
  
  real tt = r();
}

generated quantities {
  real t1 = reduce_sum(f1, y1, 1);
  real t1a = reduce_sum(f1, y1, 1) + reduce_sum(f1a, y1, 1);
  real t2 = reduce_sum(f2, y2, 1);
  real t3 = reduce_sum(f3, y3, 1);
  real t4 = reduce_sum(f4, y4, 1);
  real t5 = reduce_sum(f5, y5, 1);
  real t6 = reduce_sum(f6, y6, 1);
  real t7 = reduce_sum(f7, y7, 1);
  real t8 = reduce_sum(f8, y8, 1);
  real t9 = reduce_sum(f9, y14d, 1);
  real t10 = reduce_sum(f10, y15d, 1);
  real t11 = reduce_sum(f11, y16d, 1);
  real t12 = reduce_sum(f12, y17, 1);
  real tg1 = reduce_sum(g1, y1, 1, y9);
  real tg2 = reduce_sum(g2, y1, 1, y10);
  real tg3 = reduce_sum(g3, y1, 1, y11);
  real tg4 = reduce_sum(g4, y1, 1, y12);
  real tg5 = reduce_sum(g5, y1, 1, y1);
  real tg6 = reduce_sum(g6, y1, 1, y2);
  real tg7 = reduce_sum(g7, y1, 1, y3);
  real tg8 = reduce_sum(g8, y1, 1, y4);
  real tg9 = reduce_sum(g9, y1, 1, y5);
  real tg10 = reduce_sum(g10, y1, 1, y6);
  real tg11 = reduce_sum(g11, y1, 1, y7);
  real tg12 = reduce_sum(g12, y1, 1, y8);
  real tgs = reduce_sum(s, y1d, 1, y13d, y9d, y10d, y11d, y12d,
                        y14d, y1d, y2d, y3d, y4d,
                        y15d, y5d, y6d, y7d, y8d,
                        y16d, y17);
}
