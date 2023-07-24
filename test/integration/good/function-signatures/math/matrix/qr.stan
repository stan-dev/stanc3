data {
  int d_int;
  matrix[d_int,d_int] d_matrix;
}

transformed data {
  tuple(matrix[d_int,d_int],matrix[d_int,d_int]) QR_tdata;

  QR_tdata = qr(d_matrix);
}
parameters {
  real y_p;
  matrix[d_int,d_int] p_matrix;
}
transformed parameters {
  tuple(matrix[d_int,d_int],matrix[d_int,d_int]) QR_tparam;

  QR_tparam = qr(d_matrix);
  QR_tparam = qr(p_matrix);
}
model {
  y_p ~ normal(0,1);
}
