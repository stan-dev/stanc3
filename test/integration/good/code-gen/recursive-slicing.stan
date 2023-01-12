functions {
// reported in stanc3#1224
 vector test2(vector gamma) {
   int D = num_elements(gamma);

   if (D == 1)
      return rep_vector(D, 0);
    else
      return test2(gamma[1:D - 1]);
 }
// reported in cmdstan#1109
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

// mutual recursion
vector test4(vector gamma) {
   int D = num_elements(gamma);

   if (D == 1)
      return rep_vector(D, 0);
    else
      return test3(gamma[1:D - 1]);
  }
vector test3(vector gamma) {
  return test4(gamma);
}

// non-returning fun app
void test6(vector alpha){
   int D = num_elements(alpha);

   if (D == 1)
      print(alpha[1]);
    else
      test6(alpha[1:D - 1]);
}

 vector test7(vector gamma) {
   int D = num_elements(gamma);

   if (D == 1)
      return rep_vector(D, 0);
    else
      return test7(head(gamma,D - 1));
  }

  // recursion through higher-order function
  vector foo(real x, vector s, matrix y) {
    return ode_rk45(foo, [1]', 0.0, {1.0}, y[2:])[1];
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
  vector[3] z = foo(1.0, gamma, diag_matrix(gamma));
}
