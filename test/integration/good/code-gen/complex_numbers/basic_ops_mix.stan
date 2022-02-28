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

parameters {
  complex_matrix[N,N] cvmat;
  complex_vector[N] cvvec;
  complex_row_vector[N] cvrowvec;
  complex zv;

  matrix[N,N] vmat;
  vector[N] vvec;
  row_vector[N] vrowvec;
  real v;
}


transformed parameters {
  complex_matrix[N,N] gq_c_matrix;

  // matrix-matrix multiply and elt
  gq_c_matrix = cmat * cvmat;
  gq_c_matrix = cmat * vmat;
  gq_c_matrix = mat * cvmat;
  gq_c_matrix = cmat .* cvmat;
  gq_c_matrix = cmat .* vmat;
  gq_c_matrix = mat .* cvmat;
  gq_c_matrix = cmat ./ cvmat;
  gq_c_matrix = cmat ./ vmat;
  gq_c_matrix = mat ./ cvmat;

  // matrix-scalar multiply
  gq_c_matrix = cmat * zv;
  gq_c_matrix = z * cvmat;
  gq_c_matrix = r * cvmat;
  gq_c_matrix = cmat * v;
  gq_c_matrix = mat * zv;
  gq_c_matrix = z * vmat;

  // matrix-matrix addition and subtraction
  gq_c_matrix = cmat + cvmat;
  gq_c_matrix = cmat + vmat;
  gq_c_matrix = mat + cvmat;
  gq_c_matrix = cmat - cvmat;
  gq_c_matrix = cmat - vmat;
  gq_c_matrix = mat - cvmat;

  // vector-rowvector multiply
  gq_c_matrix = cvec * cvrowvec;
  gq_c_matrix = vec * cvrowvec;
  gq_c_matrix = cvec * vrowvec;

  complex_vector[N] gq_c_vector = cvrowvec';
  // matrix-vector products
  gq_c_vector = cmat * cvvec;
  gq_c_vector = cvmat * cvec;
  gq_c_vector = mat * cvvec;
  gq_c_vector = cmat * vvec;

  // vector-scalar multiplication
  gq_c_vector = z * cvvec;
  gq_c_vector = cvec * zv;
  gq_c_vector = r * cvvec;
  gq_c_vector = cvec * v;
  gq_c_vector = z * vvec;
  gq_c_vector = vec * zv;

  // vector-vector elt mult and div
  gq_c_vector = cvec .* cvvec;
  gq_c_vector = vec .* cvvec;
  gq_c_vector = cvec .* vvec;
  gq_c_vector = cvec ./ cvvec;
  gq_c_vector = vec ./ cvvec;
  gq_c_vector = cvec ./ vvec;

  // vector-vector addition and subtraction
  gq_c_vector = cvec + cvvec;
  gq_c_vector = vec + cvvec;
  gq_c_vector = cvec + vvec;
  gq_c_vector = cvec - cvvec;
  gq_c_vector = vec - cvvec;
  gq_c_vector = cvec - vvec;

  complex_row_vector[N] gq_c_rowvector = cvvec';
  // rowvector-matrix multiplication
  gq_c_rowvector = crowvec * cvmat;
  gq_c_rowvector = rowvec * cvmat;
  gq_c_rowvector = crowvec * vmat;

  // rowvector-scalar multiplication
  gq_c_rowvector = z * cvrowvec;
  gq_c_rowvector = crowvec * zv;
  gq_c_rowvector = r * cvrowvec;
  gq_c_rowvector = crowvec * v;
  gq_c_rowvector = z * vrowvec;
  gq_c_rowvector = rowvec * zv;

  // rowvector-rowvector elt mult and div
  gq_c_rowvector = crowvec .* cvrowvec;
  gq_c_rowvector = crowvec .* vrowvec;
  gq_c_rowvector = rowvec .* cvrowvec;
  gq_c_rowvector = crowvec ./ cvrowvec;
  gq_c_rowvector = crowvec ./ vrowvec;
  gq_c_rowvector = rowvec ./ cvrowvec;

  // rowvector-rowvector addition and subtraction
  gq_c_rowvector = crowvec + cvrowvec;
  gq_c_rowvector = crowvec + vrowvec;
  gq_c_rowvector = rowvec + cvrowvec;
  gq_c_rowvector = crowvec - cvrowvec;
  gq_c_rowvector = crowvec - vrowvec;
  gq_c_rowvector = rowvec - cvrowvec;

  complex gq_c;
  // rowvector-vector multiply
  gq_c = crowvec * cvvec;
  gq_c = cvrowvec * cvec;
  gq_c = crowvec * vvec;
  gq_c = rowvec * cvvec;

  // TODO ldivide, pow, matrix_power

  // transformations
  array[N,N] complex carray;
  carray = to_array_2d(cvmat);

}
