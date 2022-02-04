functions {
  complex_matrix foo(complex_matrix Z){
    return Z;
  }
   matrix foo(matrix A){
    reject("called the wrong foo");
  }
}
data {
  int N;
}

model {
  complex_matrix[N,N] x;
  x = foo(x);
}
