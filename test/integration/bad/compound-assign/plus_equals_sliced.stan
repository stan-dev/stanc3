functions {
  void foo_vec(int a1) {
    array[a1] int J;
    matrix[2, 2] aa;
    matrix[3, 4] bb;
    aa[J,1] += bb[1:2,1:2];
  }
}
