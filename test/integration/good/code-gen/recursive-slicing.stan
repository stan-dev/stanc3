functions {
// reported in stanc3#1224
vector test2(vector gamma);
 vector test2(vector gamma) {
   int D = num_elements(gamma);

   if (D == 1)
      return rep_vector(D, 0);
    else
      return test2(gamma[1:D - 1]);
  }
// reported in cmdstan#1109
matrix matrix_pow(matrix a, int n);
matrix matrix_pow(matrix a, int n) {
  if (n == 0) {
    return diag_matrix(rep_vector(1, rows(a)));
  } else {
    return a *  matrix_pow(a[1:rows(a), 1:cols(a)], n - 1);
  }
}

// overload which ideally *shouldn't* but will:
real foo(matrix a){
  return 1;
}

real foo(real b){
  matrix[10,10] B = rep_matrix(b,10,10);
  return foo(B[:,2:4]);
}

}
data {
  int N;
  int times;
}
parameters {
  vector[times] gamma;
}
transformed parameters {
  vector[N] z_hat = test2(gamma);
}
