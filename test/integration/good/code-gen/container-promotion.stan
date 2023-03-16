transformed data {
  array[2, 2] real Arr;
  Arr = {{1, 2}, {3, 4.5}};
}
parameters {
  real y;
}
transformed parameters {
  vector[2] V;
  V = [1, y]';
  
  array[2] row_vector[2] arRV;
  arRV = {[1, 2], [3, y]};
  
  array[2, 2] real Mar;
  Mar = {{1, 2}, {3, y}};
  
  matrix[2, 2] M;
  M = [[1, 2], [3, y]];
  
  array[2, 2, 2] real deep_Mar;
  deep_Mar = {{{0, 0}, {0, 0}}, {{1, 2}, {3, y}}};
  
  array[2] matrix[2, 2] deep_M;
  deep_M = {[[0, 0], [0, 0]], [[1, 2], [y, 4]]};
}
