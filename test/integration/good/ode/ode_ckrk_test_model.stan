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
  vector[N] zd[N] = ode_ckrk(f_12_arg, vd, rd, rad,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zd = ode_ckrk(f_11_arg, vd, rd, rad,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk(f_10_arg, vd, rd, rad,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk(f_9_arg, vd, rd, rad,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk(f_8_arg, vd, rd, rad,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zd = ode_ckrk(f_7_arg, vd, rd, rad,
                  md, rvd, vd, iad, rad, id, rd);
  zd = ode_ckrk(f_6_arg, vd, rd, rad,
                  rvd, vd, iad, rad, id, rd);
  zd = ode_ckrk(f_5_arg, vd, rd, rad,
                  vd, iad, rad, id, rd);
  zd = ode_ckrk(f_4_arg, vd, rd, rad,
                  iad, rad, id, rd);
  zd = ode_ckrk(f_3_arg, vd, rd, rad,
                  rad, id, rd);
  zd = ode_ckrk(f_2_arg, vd, rd, rad,
                  id, rd);
  zd = ode_ckrk(f_1_arg, vd, rd, rad,
                  rd);
  zd = ode_ckrk(f_0_arg, vd, rd, rad);
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
  vector[N] z[N] = ode_ckrk(f_12_arg, vd, rd, rad,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  z = ode_ckrk(f_11_arg, vd, rd, rad,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk(f_10_arg, vd, rd, rad,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk(f_9_arg, vd, rd, rad,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk(f_8_arg, vd, rd, rad,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  z = ode_ckrk(f_7_arg, vd, rd, rad,
                  md, rvd, vd, iad, rad, id, rd);
  z = ode_ckrk(f_6_arg, vd, rd, rad,
                  rvd, vd, iad, rad, id, rd);
  z = ode_ckrk(f_5_arg, vd, rd, rad,
                  vd, iad, rad, id, rd);
  z = ode_ckrk(f_4_arg, vd, rd, rad,
                  iad, rad, id, rd);
  z = ode_ckrk(f_3_arg, vd, rd, rad,
                  rad, id, rd);
  z = ode_ckrk(f_2_arg, vd, rd, rad,
                  id, rd);
  z = ode_ckrk(f_1_arg, vd, rd, rad,
                  rd);
  z = ode_ckrk(f_0_arg, vd, rd, rad);
  
  z = ode_ckrk(f_12_arg, v, r, ra,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk(f_11_arg, v, r, ra,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk(f_10_arg, v, r, ra,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk(f_9_arg, v, r, ra,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk(f_8_arg, v, r, ra,
                  raa, m, rv, v,
                  iad, ra, id, r);
  z = ode_ckrk(f_7_arg, v, r, ra,
                  m, rv, v, iad, ra, id, r);
  z = ode_ckrk(f_6_arg, v, r, ra,
                  rv, v, iad, ra, id, r);
  z = ode_ckrk(f_5_arg, v, r, ra,
                  v, iad, ra, id, r);
  z = ode_ckrk(f_4_arg, v, r, ra,
                  iad, ra, id, r);
  z = ode_ckrk(f_3_arg, v, r, ra,
                  ra, id, r);
  z = ode_ckrk(f_2_arg, v, r, ra,
                  id, r);
  z = ode_ckrk(f_1_arg, v, r, ra,
                  r);
  z = ode_ckrk(f_0_arg, v, r, ra);
}

model {
  vector[N] zm[N] = ode_ckrk(f_12_arg, vd, rd, rad,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zm = ode_ckrk(f_11_arg, vd, rd, rad,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk(f_10_arg, vd, rd, rad,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk(f_9_arg, vd, rd, rad,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk(f_8_arg, vd, rd, rad,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zm = ode_ckrk(f_7_arg, vd, rd, rad,
                  md, rvd, vd, iad, rad, id, rd);
  zm = ode_ckrk(f_6_arg, vd, rd, rad,
                  rvd, vd, iad, rad, id, rd);
  zm = ode_ckrk(f_5_arg, vd, rd, rad,
                  vd, iad, rad, id, rd);
  zm = ode_ckrk(f_4_arg, vd, rd, rad,
                  iad, rad, id, rd);
  zm = ode_ckrk(f_3_arg, vd, rd, rad,
                  rad, id, rd);
  zm = ode_ckrk(f_2_arg, vd, rd, rad,
                  id, rd);
  zm = ode_ckrk(f_1_arg, vd, rd, rad,
                  rd);
  zm = ode_ckrk(f_0_arg, vd, rd, rad);
  
  zm = ode_ckrk(f_12_arg, v, r, ra,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk(f_11_arg, v, r, ra,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk(f_10_arg, v, r, ra,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk(f_9_arg, v, r, ra,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk(f_8_arg, v, r, ra,
                  raa, m, rv, v,
                  iad, ra, id, r);
  zm = ode_ckrk(f_7_arg, v, r, ra,
                  m, rv, v, iad, ra, id, r);
  zm = ode_ckrk(f_6_arg, v, r, ra,
                  rv, v, iad, ra, id, r);
  zm = ode_ckrk(f_5_arg, v, r, ra,
                  v, iad, ra, id, r);
  zm = ode_ckrk(f_4_arg, v, r, ra,
                  iad, ra, id, r);
  zm = ode_ckrk(f_3_arg, v, r, ra,
                  ra, id, r);
  zm = ode_ckrk(f_2_arg, v, r, ra,
                  id, r);
  zm = ode_ckrk(f_1_arg, v, r, ra,
                  r);
  zm = ode_ckrk(f_0_arg, v, r, ra);
  
  r ~ normal(0, 1);
}

generated quantities {
  vector[N] zg[N] = ode_ckrk(f_12_arg, vd, rd, rad,
                               mad, rvad, vad, iaad, raad, md, rvd, vd,
                               iad, rad, id, rd);
  zg = ode_ckrk(f_11_arg, vd, rd, rad,
                  rvad, vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk(f_10_arg, vd, rd, rad,
                  vad, iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk(f_9_arg, vd, rd, rad,
                  iaad, raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk(f_8_arg, vd, rd, rad,
                  raad, md, rvd, vd,
                  iad, rad, id, rd);
  zg = ode_ckrk(f_7_arg, vd, rd, rad,
                  md, rvd, vd, iad, rad, id, rd);
  zg = ode_ckrk(f_6_arg, vd, rd, rad,
                  rvd, vd, iad, rad, id, rd);
  zg = ode_ckrk(f_5_arg, vd, rd, rad,
                  vd, iad, rad, id, rd);
  zg = ode_ckrk(f_4_arg, vd, rd, rad,
                  iad, rad, id, rd);
  zg = ode_ckrk(f_3_arg, vd, rd, rad,
                  rad, id, rd);
  zg = ode_ckrk(f_2_arg, vd, rd, rad,
                  id, rd);
  zg = ode_ckrk(f_1_arg, vd, rd, rad,
                  rd);
  zg = ode_ckrk(f_0_arg, vd, rd, rad);
  
  zg = ode_ckrk(f_12_arg, v, r, ra,
                  ma, rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk(f_11_arg, v, r, ra,
                  rva, va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk(f_10_arg, v, r, ra,
                  va, iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk(f_9_arg, v, r, ra,
                  iaad, raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk(f_8_arg, v, r, ra,
                  raa, m, rv, v,
                  iad, ra, id, r);
  zg = ode_ckrk(f_7_arg, v, r, ra,
                  m, rv, v, iad, ra, id, r);
  zg = ode_ckrk(f_6_arg, v, r, ra,
                  rv, v, iad, ra, id, r);
  zg = ode_ckrk(f_5_arg, v, r, ra,
                  v, iad, ra, id, r);
  zg = ode_ckrk(f_4_arg, v, r, ra,
                  iad, ra, id, r);
  zg = ode_ckrk(f_3_arg, v, r, ra,
                  ra, id, r);
  zg = ode_ckrk(f_2_arg, v, r, ra,
                  id, r);
  zg = ode_ckrk(f_1_arg, v, r, ra,
                  r);
  zg = ode_ckrk(f_0_arg, v, r, ra);
}