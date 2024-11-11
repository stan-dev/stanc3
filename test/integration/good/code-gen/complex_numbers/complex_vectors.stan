transformed data {
  int n = 1;
}
transformed parameters {
  complex_vector[3] z = [3, 4i, 4]';
  complex_matrix[2, 2] zs = [[1, 2], [3, 4]];
  row_vector[2] x = [1, 2];
  complex_row_vector[2] zx = x;
  complex_matrix[n + 1, n + 1] cm = [[1, 2], [3, 4i]];
  cm = [zx, cm[1]];

  array[2] complex_matrix[2, 2] acm = {[[1, 2], [3, 4]], cm};
  array[2] complex_vector[3] az = {z, [1, 2, 3]'};
}
