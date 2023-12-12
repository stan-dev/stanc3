generated quantities {
  matrix[1, 1] m = [[1.5]];
  // m * 2.0 is a temporary:
  tuple(real, matrix[1, 1]) t = (1.2, m * 2.0);
  // m is both data and an Eigen type, but not a map:
  tuple(real, matrix[1, 1]) s = (t.1, m);
}
