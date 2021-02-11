data {
  int a;
  real b;
  vector[2] c;
  row_vector[3] d;
  matrix[2,2] e;
  int f[5];
  real g[6];
  vector[1] h[7];
  matrix[2,2] i[2];
  int j[3,1,3];
}

transformed data {
  int k = a + 1;
}

parameters {
  simplex[10] l;
  unit_vector[11] m;
  ordered[12] n;
  positive_ordered[13] o;
  cov_matrix[14] p;
  corr_matrix[15] q;
  cholesky_factor_cov[16] r;
  cholesky_factor_corr[17] s;
}

transformed parameters {
  matrix[14, 14] t = p;
  {
    real ignored_tp = log(r[0][0]);
  }
}

model {
    real ignored_model = square(r[0][0]);
    l ~ dirichlet(c);
}

generated quantities {
  real u = l[0];
  {
    real ignored_gq = l[1];
  }
}
