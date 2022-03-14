data {
  int N;
}
parameters {
  complex_matrix[N,N] cmat;
  complex_vector[N] cvec;
  complex_row_vector[N] crowvec;
  complex z;

  matrix[N,N] mat;
  vector[N] vec;
  row_vector[N] rowvec;
  real r;
}


transformed parameters {
  complex_matrix[N,N] tp_c_matrix;

  // matrix-matrix multiply and elt
  tp_c_matrix = cmat * cmat;
  tp_c_matrix = cmat * mat;
  tp_c_matrix = mat * cmat;
  tp_c_matrix = cmat .* cmat;
  tp_c_matrix = cmat .* mat;
  tp_c_matrix = mat .* cmat;
  tp_c_matrix = cmat ./ cmat;
  tp_c_matrix = cmat ./ mat;
  tp_c_matrix = mat ./ cmat;

  // matrix-scalar multiply
  tp_c_matrix = cmat * z;
  tp_c_matrix = z * cmat;
  tp_c_matrix = r * cmat;
  tp_c_matrix = cmat * r;
  tp_c_matrix = mat * z;
  tp_c_matrix = z * mat;

  // matrix-matrix addition and subtraction
  tp_c_matrix = cmat + cmat;
  tp_c_matrix = cmat + mat;
  tp_c_matrix = mat + cmat;
  tp_c_matrix = cmat - cmat;
  tp_c_matrix = cmat - mat;
  tp_c_matrix = mat - cmat;
  tp_c_matrix = -cmat;
  tp_c_matrix = -mat;

  // vector-rowvector multiply
  tp_c_matrix = cvec * crowvec;
  tp_c_matrix = vec * crowvec;
  tp_c_matrix = cvec * rowvec;

  complex_vector[N] tp_c_vector = crowvec';
  // matrix-vector products
  tp_c_vector = cmat * cvec;
  tp_c_vector = mat * cvec;
  tp_c_vector = cmat * vec;

  // vector-scalar multiplication
  tp_c_vector = z * cvec;
  tp_c_vector = cvec * z;
  tp_c_vector = r * cvec;
  tp_c_vector = cvec * r;
  tp_c_vector = z * vec;
  tp_c_vector = vec * z;

  // vector-vector elt mult and div
  tp_c_vector = cvec .* cvec;
  tp_c_vector = vec .* cvec;
  tp_c_vector = cvec .* vec;
  tp_c_vector = cvec ./ cvec;
  tp_c_vector = vec ./ cvec;
  tp_c_vector = cvec ./ vec;

  // vector-vector addition and subtraction
  tp_c_vector = cvec + cvec;
  tp_c_vector = vec + cvec;
  tp_c_vector = cvec + vec;
  tp_c_vector = cvec - cvec;
  tp_c_vector = vec - cvec;
  tp_c_vector = cvec - vec;
  tp_c_vector = -cvec;
  tp_c_vector = -vec;

  complex_row_vector[N] tp_c_rowvector = cvec';
  // rowvector-matrix multiplication
  tp_c_rowvector = crowvec * cmat;
  tp_c_rowvector = rowvec * cmat;
  tp_c_rowvector = crowvec * mat;

  // rowvector-scalar multiplication
  tp_c_rowvector = z * crowvec;
  tp_c_rowvector = crowvec * z;
  tp_c_rowvector = r * crowvec;
  tp_c_rowvector = crowvec * r;
  tp_c_rowvector = z * rowvec;
  tp_c_rowvector = rowvec * z;

  // rowvector-rowvector elt mult and div
  tp_c_rowvector = crowvec .* crowvec;
  tp_c_rowvector = crowvec .* rowvec;
  tp_c_rowvector = rowvec .* crowvec;
  tp_c_rowvector = crowvec ./ crowvec;
  tp_c_rowvector = crowvec ./ rowvec;
  tp_c_rowvector = rowvec ./ crowvec;

  // rowvector-rowvector addition and subtraction
  tp_c_rowvector = crowvec + crowvec;
  tp_c_rowvector = crowvec + rowvec;
  tp_c_rowvector = rowvec + crowvec;
  tp_c_rowvector = crowvec - crowvec;
  tp_c_rowvector = crowvec - rowvec;
  tp_c_rowvector = rowvec - crowvec;
  tp_c_rowvector = -crowvec;
  tp_c_rowvector = -rowvec;

  complex tp_c;
  // rowvector-vector multiply
  tp_c = crowvec * cvec;
  tp_c = crowvec * vec;
  tp_c = rowvec * cvec;

  // reductions
  tp_c = sum(to_array_1d(cvec));

  // TODO ldivide, pow, matrix_power

  // transformations
  array[N,N] complex carray;
  carray = to_array_2d(cmat);

}
