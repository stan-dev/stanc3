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
  gq_c_matrix = multiply(cmat, mat);
  gq_c_matrix = multiply(mat, cmat);
  gq_c_matrix = cmat .* cmat;
  gq_c_matrix = elt_multiply(cmat, mat);
  gq_c_matrix = elt_multiply(mat,cmat);
  // todo more here
}
