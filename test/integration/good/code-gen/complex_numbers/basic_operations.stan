data {
  int N;
  complex_matrix[N,N] cmat;
  complex_vector[N] cvec;
  complex_row_vector[N] crowvec;
  complex z;

  matrix[N,N] mat;
  vector[N] vec;
  row_vector[N] rowvec;
  real r;
}


generated quantities {
  complex_matrix[N,N] gq_c_matrix;

  // matrix-matrix multiply
  gq_c_matrix = cmat * cmat;
  gq_c_matrix = cmat * mat;
  gq_c_matrix = mat * cmat;
  gq_c_matrix = cmat .* cmat;
  gq_c_matrix = cmat .* mat;
  gq_c_matrix = mat .* cmat;

  // matrix-scalar multiply
  gq_c_matrix = cmat * z;
  gq_c_matrix = z * cmat;
  gq_c_matrix = r * cmat;
  gq_c_matrix = cmat * r;
  gq_c_matrix = mat * z;
  gq_c_matrix = z * mat;

  // matrix-matrix addition and subtraction
  gq_c_matrix = cmat + cmat;
  gq_c_matrix = cmat + mat;
  gq_c_matrix = mat + cmat;
  gq_c_matrix = cmat - cmat;
  gq_c_matrix = cmat - mat;
  gq_c_matrix = mat - cmat;
  // TODO more here
}
