functions {}
data {
  int<lower=1> N;
  matrix[N, N] K;
  row_vector[N] row_vec;
  vector[N] vec;
  int scalar;

  complex_matrix[N,N] C;
  complex_row_vector[N] crow_vec;
  complex_vector[N] cvec;
  complex cscalar;
}
transformed data {
  matrix[N, N] K1;
  matrix[N, N] K2;
  matrix[N, N] K3;
  K1 = add_diag(K, row_vec);
  K2 = add_diag(K, vec);
  K3 = add_diag(K, scalar);

  complex_matrix[N, N] tdcK;
  tdcK = add_diag(C, crow_vec);
  tdcK = add_diag(C, cvec);
  tdcK = add_diag(C, scalar);
}
parameters {
  matrix[N, N] Kp;
  row_vector[N] row_vec_p;
  vector[N] vec_p;
  real scalar_p;


  complex_matrix[N,N] Cp;
  complex_row_vector[N] crow_vec_p;
  complex_vector[N] cvec_p;
  complex cscalar_p;
}
model {
  matrix[N, N] K4 = add_diag(Kp, row_vec_p);
  matrix[N, N] K5 = add_diag(Kp, vec_p);
  matrix[N, N] K6 = add_diag(Kp, scalar_p);
  matrix[N, N] K7 = add_diag(K, row_vec_p);
  matrix[N, N] K8 = add_diag(K, vec_p);
  matrix[N, N] K9 = add_diag(K, scalar_p);
  matrix[N, N] K10 = add_diag(Kp, row_vec);
  matrix[N, N] K11 = add_diag(Kp, vec);
  matrix[N, N] K12 = add_diag(Kp, scalar);

  complex_matrix[N,N] tpcK;
  tpcK = add_diag(Cp, crow_vec_p);
  tpcK = add_diag(Cp, cvec_p);
  tpcK = add_diag(Cp, cscalar_p);
  tpcK = add_diag(C, crow_vec_p);
  tpcK = add_diag(C, cvec_p);
  tpcK = add_diag(C, cscalar_p);
  tpcK = add_diag(Cp, crow_vec);
  tpcK = add_diag(Cp, cvec);
  tpcK = add_diag(Cp, cscalar);

}
generated quantities {}
