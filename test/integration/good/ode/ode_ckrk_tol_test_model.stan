functions {
  vector f_0_arg(real t, vector z) {
    return z;
  }
  vector f_1_arg(real t, vector z, real a) {
    return z;
  }
  vector f_2_arg(real t, vector z, int b, real a) {
    return z;
  }
  vector f_3_arg(real t, vector z, real[] c, int b, real a) {
    return z;
  }
  vector f_4_arg(real t, vector z, int[] d, real[] c, int b, real a) {
    return z;
  }
  vector f_5_arg(real t, vector z, vector e, int[] d, real[] c, int b, real a) {
    return z;
  }
  vector f_6_arg(real t, vector z, row_vector f, vector e, int[] d, real[] c,
                 int b, real a) {
    return z;
  }
  vector f_7_arg(real t, vector z, matrix g, row_vector f, vector e, int[] d,
                 real[] c, int b, real a) {
    return z;
  }
  vector f_8_arg(real t, vector z, real[,] h, matrix g, row_vector f, vector e,
                 int[] d, real[] c, int b, real a) {
    return z;
  }
  vector f_9_arg(real t, vector z, int[,] i, real[,] h, matrix g, row_vector f,
                 vector e, int[] d, real[] c, int b, real a) {
    return z;
  }
  vector f_10_arg(real t, vector z, vector[] j, int[,] i, real[,] h, matrix g,
                  row_vector f, vector e, int[] d, real[] c, int b, real a) {
    return z;
  }
  vector f_11_arg(real t, vector z, row_vector[] k, vector[] j, int[,] i,
                  real[,] h, matrix g, row_vector f, vector e, int[] d, real[] c,
                  int b, real a) {
    return z;
  }
  vector f_12_arg(real t, vector z, matrix[] l, row_vector[] k, vector[] j,
                  int[,] i, real[,] h, matrix g, row_vector f, vector e,
                  int[] d, real[] c, int b, real a) {
    return z;
  }
}

data {
  int N;
  int id;
  real rd;
  real rad[N];
  int iad[N];
  vector[N] vd;
  row_vector[N] rvd;
  matrix[N, N] md;
  real raad[N, N];
  int iaad[N, N];
  vector[N] vad[N];
  row_vector[N] rvad[N];
  matrix[N, N] mad[N];
}

transformed data {
  vector[N] zd[N] = ode_ckrk_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zd = ode_ckrk_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  md, rvd, vd, iad, rad, id, rd);
  zd = ode_ckrk_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvd, vd, iad, rad, id, rd);
  zd = ode_ckrk_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vd, iad, rad, id, rd);
  zd = ode_ckrk_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iad, rad, id, rd);
  zd = ode_ckrk_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rad, id, rd);
  zd = ode_ckrk_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  id, rd);
  zd = ode_ckrk_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rd);
  zd = ode_ckrk_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
}



parameters {
  real r;
  real ra[N];
  vector[N] v;
  row_vector[N] rv;
  matrix[N, N] m;
  real raa[N, N];
  vector[N] va[N];
  row_vector[N] rva[N];
  matrix[N, N] ma[N];
}

transformed parameters {
  vector[N] z[N] = ode_ckrk_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  z = ode_ckrk_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  md, rvd, vd, iad, rad, id, rd);
  z = ode_ckrk_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvd, vd, iad, rad, id, rd);
  z = ode_ckrk_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vd, iad, rad, id, rd);
  z = ode_ckrk_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iad, rad, id, rd);
  z = ode_ckrk_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rad, id, rd);
  z = ode_ckrk_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  id, rd);
  z = ode_ckrk_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rd);
  z = ode_ckrk_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
  
  z = ode_ckrk_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100,
                  raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100,
                  m, rv, v, iad, ra, id, r);
  z = ode_ckrk_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rv, v, iad, ra, id, r);
  z = ode_ckrk_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100,
                  v, iad, ra, id, r);
  z = ode_ckrk_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iad, ra, id, r);
  z = ode_ckrk_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ra, id, r);
  z = ode_ckrk_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100,
                  id, r);
  z = ode_ckrk_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100,
                  r);
  z = ode_ckrk_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
}

model {
  vector[N] zm[N] = ode_ckrk_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zm = ode_ckrk_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  md, rvd, vd, iad, rad, id, rd);
  zm = ode_ckrk_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvd, vd, iad, rad, id, rd);
  zm = ode_ckrk_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vd, iad, rad, id, rd);
  zm = ode_ckrk_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iad, rad, id, rd);
  zm = ode_ckrk_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rad, id, rd);
  zm = ode_ckrk_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  id, rd);
  zm = ode_ckrk_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rd);
  zm = ode_ckrk_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);

  zm = ode_ckrk_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100,
                  raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100,
                  m, rv, v, iad, ra, id, r);
  zm = ode_ckrk_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rv, v, iad, ra, id, r);
  zm = ode_ckrk_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100,
                  v, iad, ra, id, r);
  zm = ode_ckrk_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iad, ra, id, r);
  zm = ode_ckrk_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ra, id, r);
  zm = ode_ckrk_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100,
                  id, r);
  zm = ode_ckrk_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100,
                  r);
  zm = ode_ckrk_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
  r ~ normal(0, 1);
}

generated quantities {
  vector[N] zg[N] = ode_ckrk_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zg = ode_ckrk_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  md, rvd, vd, iad, rad, id, rd);
  zg = ode_ckrk_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rvd, vd, iad, rad, id, rd);
  zg = ode_ckrk_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  vd, iad, rad, id, rd);
  zg = ode_ckrk_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  iad, rad, id, rd);
  zg = ode_ckrk_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rad, id, rd);
  zg = ode_ckrk_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  id, rd);
  zg = ode_ckrk_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100,
                  rd);
  zg = ode_ckrk_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
  zg = ode_ckrk_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100,
                  raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100,
                  m, rv, v, iad, ra, id, r);
  zg = ode_ckrk_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100,
                  rv, v, iad, ra, id, r);
  zg = ode_ckrk_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100,
                  v, iad, ra, id, r);
  zg = ode_ckrk_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100,
                  iad, ra, id, r);
  zg = ode_ckrk_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100,
                  ra, id, r);
  zg = ode_ckrk_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100,
                  id, r);
  zg = ode_ckrk_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100,
                  r);
  zg = ode_ckrk_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
}