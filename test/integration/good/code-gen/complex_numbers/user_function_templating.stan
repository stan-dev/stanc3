functions {
  complex_matrix foo(complex_matrix Z){
    return Z;
  }
   matrix foo(matrix A){
    reject("called the wrong foo");
  }
    complex_vector foo(complex_vector Z){
    return Z;
  }
   vector foo(vector A){
    reject("called the wrong foo");
  }
    complex_row_vector foo(complex_row_vector Z){
    return Z;
  }
   row_vector foo(row_vector A){
    reject("called the wrong foo");
  }
  array[] complex_matrix foo(array[] complex_matrix Z){
    return Z;
  }
  array[] matrix foo(array[] matrix A){
    reject("called the wrong foo");
  }
}
transformed data {
  int N=3;
}

model {
  complex_matrix[N,N] x;
  x = foo(x);
  complex_vector[N] y;
  y = foo(y);
  complex_row_vector[N] z;
  z = foo(z);
  array[2] complex_matrix[N,N] w = foo({x,x});
}
