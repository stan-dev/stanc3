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

  // matrix-matrix multiply and elt
  gq_c_matrix = cmat * cmat;
  gq_c_matrix = cmat * mat;
  gq_c_matrix = mat * cmat;
  gq_c_matrix = cmat .* cmat;
  gq_c_matrix = cmat .* mat;
  gq_c_matrix = mat .* cmat;
  gq_c_matrix = cmat ./ cmat;
  gq_c_matrix = cmat ./ mat;
  gq_c_matrix = mat ./ cmat;

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
  gq_c_matrix = -cmat;
  gq_c_matrix = -mat;

  // vector-rowvector multiply
  gq_c_matrix = cvec * crowvec;
  gq_c_matrix = vec * crowvec;
  gq_c_matrix = cvec * rowvec;

  complex_vector[N] gq_c_vector = crowvec';
  // matrix-vector products
  gq_c_vector = cmat * cvec;
  gq_c_vector = mat * cvec;
  gq_c_vector = cmat * vec;

  // vector-scalar multiplication
  gq_c_vector = z * cvec;
  gq_c_vector = cvec * z;
  gq_c_vector = r * cvec;
  gq_c_vector = cvec * r;
  gq_c_vector = z * vec;
  gq_c_vector = vec * z;

  // vector-vector elt mult and div
  gq_c_vector = cvec .* cvec;
  gq_c_vector = vec .* cvec;
  gq_c_vector = cvec .* vec;
  gq_c_vector = cvec ./ cvec;
  gq_c_vector = vec ./ cvec;
  gq_c_vector = cvec ./ vec;

  // vector-vector addition and subtraction
  gq_c_vector = cvec + cvec;
  gq_c_vector = vec + cvec;
  gq_c_vector = cvec + vec;
  gq_c_vector = cvec - cvec;
  gq_c_vector = vec - cvec;
  gq_c_vector = cvec - vec;
  gq_c_vector = -cvec;
  gq_c_vector = -vec;

  complex_row_vector[N] gq_c_rowvector = cvec';
  // rowvector-matrix multiplication
  gq_c_rowvector = crowvec * cmat;
  gq_c_rowvector = rowvec * cmat;
  gq_c_rowvector = crowvec * mat;

  // rowvector-scalar multiplication
  gq_c_rowvector = z * crowvec;
  gq_c_rowvector = crowvec * z;
  gq_c_rowvector = r * crowvec;
  gq_c_rowvector = crowvec * r;
  gq_c_rowvector = z * rowvec;
  gq_c_rowvector = rowvec * z;

  // rowvector-rowvector elt mult and div
  gq_c_rowvector = crowvec .* crowvec;
  gq_c_rowvector = crowvec .* rowvec;
  gq_c_rowvector = rowvec .* crowvec;
  gq_c_rowvector = crowvec ./ crowvec;
  gq_c_rowvector = crowvec ./ rowvec;
  gq_c_rowvector = rowvec ./ crowvec;

  // rowvector-rowvector addition and subtraction
  gq_c_rowvector = crowvec + crowvec;
  gq_c_rowvector = crowvec + rowvec;
  gq_c_rowvector = rowvec + crowvec;
  gq_c_rowvector = crowvec - crowvec;
  gq_c_rowvector = crowvec - rowvec;
  gq_c_rowvector = rowvec - crowvec;
  gq_c_rowvector = -crowvec;
  gq_c_rowvector = -rowvec;

  complex gq_c;
  // rowvector-vector multiply
  gq_c = crowvec * cvec;
  gq_c = crowvec * vec;
  gq_c = rowvec * cvec;

  // reductions
  gq_c = sum(to_array_1d(cvec));

  // TODO ldivide, pow, matrix_power

  // transformations
  array[N,N] complex carray;
  carray = to_array_2d(cmat);

}
