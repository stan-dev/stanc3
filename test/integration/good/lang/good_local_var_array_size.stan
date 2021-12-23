data {
  array[5] int M;
}
transformed data {
  array[2] int N;
  N[1] = 1;
  N[2] = 4;
}
parameters {
  real y;
}
model {
  y ~ normal(0, 1);
  
  for (i in 1 : 10) {
    array[i] real x; // should allow i here.
    for (j in 1 : i) 
      x[j] = j * j;
  }
  
  for (i in 1 : 5) {
    vector[i] v;
    row_vector[i] rv;
    for (j in 1 : 10) {
      matrix[i, j] m;
    }
  }
  
  for (i in 1 : 5) {
    array[M[i]] real x; // data
  }
  
  for (i in 1 : 2) {
    array[N[i]] real x; // tdata
  }
}

