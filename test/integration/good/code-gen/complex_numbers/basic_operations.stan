data {
  int N;
  complex_matrix[N,N] cmat;
  complex_vector[N] cvec;
  complex_row_vector[N] crowvec;

  matrix[N,N] mat;
  vector[N] vec;
  row_vector[N] rowvec;
}


generated quantities {
  complex_matrix[N,N] gq_c_matrix;
  gq_c_matrix = cmat * cmat;
  gq_c_matrix = cmat * mat;
  gq_c_matrix = mat * cmat;
  gq_c_matrix = cmat .* cmat;
  gq_c_matrix = cmat .* mat;
  gq_c_matrix = mat .* cmat;

  gq_c_matrix = cmat + cmat;
  gq_c_matrix = cmat + mat;
  gq_c_matrix = mat + cmat;
  gq_c_matrix = cmat - cmat;
  gq_c_matrix = cmat - mat;
  gq_c_matrix = mat - cmat;
  // TODO more here
}
