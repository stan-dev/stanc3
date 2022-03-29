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
  complex_matrix[N,N] tp_c_matrix;

  // matrix-matrix multiply and elt
  tp_c_matrix = cmat * cvmat;
  tp_c_matrix = cmat * vmat;
  tp_c_matrix = mat * cvmat;
  tp_c_matrix = cmat .* cvmat;
  tp_c_matrix = cmat .* vmat;
  tp_c_matrix = mat .* cvmat;
  tp_c_matrix = cmat ./ cvmat;
  tp_c_matrix = cmat ./ vmat;
  tp_c_matrix = mat ./ cvmat;

  // matrix-scalar multiply
  tp_c_matrix = cmat * zv;
  tp_c_matrix = z * cvmat;
  tp_c_matrix = r * cvmat;
  tp_c_matrix = cmat * v;
  tp_c_matrix = mat * zv;
  tp_c_matrix = z * vmat;

  // matrix-matrix addition and subtraction
  tp_c_matrix = cmat + cvmat;
  tp_c_matrix = cmat + vmat;
  tp_c_matrix = mat + cvmat;
  tp_c_matrix = cmat - cvmat;
  tp_c_matrix = cmat - vmat;
  tp_c_matrix = mat - cvmat;

  // vector-rowvector multiply
  tp_c_matrix = cvec * cvrowvec;
  tp_c_matrix = vec * cvrowvec;
  tp_c_matrix = cvec * vrowvec;

  complex_vector[N] tp_c_vector = cvrowvec';
  // matrix-vector products
  tp_c_vector = cmat * cvvec;
  tp_c_vector = cvmat * cvec;
  tp_c_vector = mat * cvvec;
  tp_c_vector = cmat * vvec;

  // vector-scalar multiplication
  tp_c_vector = z * cvvec;
  tp_c_vector = cvec * zv;
  tp_c_vector = r * cvvec;
  tp_c_vector = cvec * v;
  tp_c_vector = z * vvec;
  tp_c_vector = vec * zv;

  // vector-vector elt mult and div
  tp_c_vector = cvec .* cvvec;
  tp_c_vector = vec .* cvvec;
  tp_c_vector = cvec .* vvec;
  tp_c_vector = cvec ./ cvvec;
  tp_c_vector = vec ./ cvvec;
  tp_c_vector = cvec ./ vvec;

  // vector-vector addition and subtraction
  tp_c_vector = cvec + cvvec;
  tp_c_vector = vec + cvvec;
  tp_c_vector = cvec + vvec;
  tp_c_vector = cvec - cvvec;
  tp_c_vector = vec - cvvec;
  tp_c_vector = cvec - vvec;

  complex_row_vector[N] tp_c_rowvector = cvvec';
  // rowvector-matrix multiplication
  tp_c_rowvector = crowvec * cvmat;
  tp_c_rowvector = rowvec * cvmat;
  tp_c_rowvector = crowvec * vmat;

  // rowvector-scalar multiplication
  tp_c_rowvector = z * cvrowvec;
  tp_c_rowvector = crowvec * zv;
  tp_c_rowvector = r * cvrowvec;
  tp_c_rowvector = crowvec * v;
  tp_c_rowvector = z * vrowvec;
  tp_c_rowvector = rowvec * zv;

  // rowvector-rowvector elt mult and div
  tp_c_rowvector = crowvec .* cvrowvec;
  tp_c_rowvector = crowvec .* vrowvec;
  tp_c_rowvector = rowvec .* cvrowvec;
  tp_c_rowvector = crowvec ./ cvrowvec;
  tp_c_rowvector = crowvec ./ vrowvec;
  tp_c_rowvector = rowvec ./ cvrowvec;

  // rowvector-rowvector addition and subtraction
  tp_c_rowvector = crowvec + cvrowvec;
  tp_c_rowvector = crowvec + vrowvec;
  tp_c_rowvector = rowvec + cvrowvec;
  tp_c_rowvector = crowvec - cvrowvec;
  tp_c_rowvector = crowvec - vrowvec;
  tp_c_rowvector = rowvec - cvrowvec;

  complex tp_c;
  // rowvector-vector multiply
  tp_c = crowvec * cvvec;
  tp_c = cvrowvec * cvec;
  tp_c = crowvec * vvec;
  tp_c = rowvec * cvvec;

  // TODO ldivide, pow, matrix_power

  // transformations
  array[N,N] complex carray;
  carray = to_array_2d(cvmat);

}
