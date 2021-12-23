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
  vector f_3_arg(real t, vector z, array[] real c, int b, real a) {
    return z;
  }
  vector f_4_arg(real t, vector z, array[] int d, array[] real c, int b,
                 real a) {
    return z;
  }
  vector f_5_arg(real t, vector z, vector e, array[] int d, array[] real c,
                 int b, real a) {
    return z;
  }
  vector f_6_arg(real t, vector z, row_vector f, vector e, array[] int d,
                 array[] real c, int b, real a) {
    return z;
  }
  vector f_7_arg(real t, vector z, matrix g, row_vector f, vector e,
                 array[] int d, array[] real c, int b, real a) {
    return z;
  }
  vector f_8_arg(real t, vector z, array[,] real h, matrix g, row_vector f,
                 vector e, array[] int d, array[] real c, int b, real a) {
    return z;
  }
  vector f_9_arg(real t, vector z, array[,] int i, array[,] real h, matrix g,
                 row_vector f, vector e, array[] int d, array[] real c,
                 int b, real a) {
    return z;
  }
  vector f_10_arg(real t, vector z, array[] vector j, array[,] int i,
                  array[,] real h, matrix g, row_vector f, vector e,
                  array[] int d, array[] real c, int b, real a) {
    return z;
  }
  vector f_11_arg(real t, vector z, array[] row_vector k, array[] vector j,
                  array[,] int i, array[,] real h, matrix g, row_vector f,
                  vector e, array[] int d, array[] real c, int b, real a) {
    return z;
  }
  vector f_12_arg(real t, vector z, array[] matrix l, array[] row_vector k,
                  array[] vector j, array[,] int i, array[,] real h,
                  matrix g, row_vector f, vector e, array[] int d,
                  array[] real c, int b, real a) {
    return z;
  }
}
data {
  int N;
  int id;
  real rd;
  array[N] real rad;
  array[N] int iad;
  vector[N] vd;
  row_vector[N] rvd;
  matrix[N, N] md;
  array[N, N] real raad;
  array[N, N] int iaad;
  array[N] vector[N] vad;
  array[N] row_vector[N] rvad;
  array[N] matrix[N, N] mad;
}
transformed data {
  array[N] vector[N] zd = ode_adams_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6,
                                        100, mad, rvad, vad, iaad, raad, md,
                                        rvd, vd, iad, rad, id, rd);
  zd = ode_adams_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvad, vad, iaad,
                     raad, md, rvd, vd, iad, rad, id, rd);
  zd = ode_adams_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100, vad, iaad, raad,
                     md, rvd, vd, iad, rad, id, rd);
  zd = ode_adams_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100, iaad, raad, md,
                     rvd, vd, iad, rad, id, rd);
  zd = ode_adams_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100, raad, md, rvd,
                     vd, iad, rad, id, rd);
  zd = ode_adams_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100, md, rvd, vd, iad,
                     rad, id, rd);
  zd = ode_adams_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvd, vd, iad,
                     rad, id, rd);
  zd = ode_adams_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100, vd, iad, rad, id,
                     rd);
  zd = ode_adams_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100, iad, rad, id, rd);
  zd = ode_adams_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100, rad, id, rd);
  zd = ode_adams_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100, id, rd);
  zd = ode_adams_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100, rd);
  zd = ode_adams_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
}
parameters {
  real r;
  array[N] real ra;
  vector[N] v;
  row_vector[N] rv;
  matrix[N, N] m;
  array[N, N] real raa;
  array[N] vector[N] va;
  array[N] row_vector[N] rva;
  array[N] matrix[N, N] ma;
}
transformed parameters {
  array[N] vector[N] z = ode_adams_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6,
                                       100, mad, rvad, vad, iaad, raad, md,
                                       rvd, vd, iad, rad, id, rd);
  z = ode_adams_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvad, vad, iaad,
                    raad, md, rvd, vd, iad, rad, id, rd);
  z = ode_adams_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100, vad, iaad, raad,
                    md, rvd, vd, iad, rad, id, rd);
  z = ode_adams_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100, iaad, raad, md,
                    rvd, vd, iad, rad, id, rd);
  z = ode_adams_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100, raad, md, rvd, vd,
                    iad, rad, id, rd);
  z = ode_adams_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100, md, rvd, vd, iad,
                    rad, id, rd);
  z = ode_adams_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvd, vd, iad, rad,
                    id, rd);
  z = ode_adams_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100, vd, iad, rad, id,
                    rd);
  z = ode_adams_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100, iad, rad, id, rd);
  z = ode_adams_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100, rad, id, rd);
  z = ode_adams_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100, id, rd);
  z = ode_adams_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100, rd);
  z = ode_adams_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
  
  z = ode_adams_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100, ma, rva, va, iaad,
                    raa, m, rv, v, iad, ra, id, r);
  z = ode_adams_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100, rva, va, iaad, raa,
                    m, rv, v, iad, ra, id, r);
  z = ode_adams_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100, va, iaad, raa, m,
                    rv, v, iad, ra, id, r);
  z = ode_adams_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100, iaad, raa, m, rv, v,
                    iad, ra, id, r);
  z = ode_adams_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100, raa, m, rv, v, iad,
                    ra, id, r);
  z = ode_adams_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100, m, rv, v, iad, ra,
                    id, r);
  z = ode_adams_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100, rv, v, iad, ra, id,
                    r);
  z = ode_adams_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100, v, iad, ra, id, r);
  z = ode_adams_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100, iad, ra, id, r);
  z = ode_adams_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100, ra, id, r);
  z = ode_adams_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100, id, r);
  z = ode_adams_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100, r);
  z = ode_adams_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
}
model {
  array[N] vector[N] zm = ode_adams_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6,
                                        100, mad, rvad, vad, iaad, raad, md,
                                        rvd, vd, iad, rad, id, rd);
  zm = ode_adams_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvad, vad, iaad,
                     raad, md, rvd, vd, iad, rad, id, rd);
  zm = ode_adams_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100, vad, iaad, raad,
                     md, rvd, vd, iad, rad, id, rd);
  zm = ode_adams_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100, iaad, raad, md,
                     rvd, vd, iad, rad, id, rd);
  zm = ode_adams_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100, raad, md, rvd,
                     vd, iad, rad, id, rd);
  zm = ode_adams_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100, md, rvd, vd, iad,
                     rad, id, rd);
  zm = ode_adams_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvd, vd, iad,
                     rad, id, rd);
  zm = ode_adams_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100, vd, iad, rad, id,
                     rd);
  zm = ode_adams_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100, iad, rad, id, rd);
  zm = ode_adams_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100, rad, id, rd);
  zm = ode_adams_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100, id, rd);
  zm = ode_adams_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100, rd);
  zm = ode_adams_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
  
  zm = ode_adams_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100, ma, rva, va, iaad,
                     raa, m, rv, v, iad, ra, id, r);
  zm = ode_adams_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100, rva, va, iaad, raa,
                     m, rv, v, iad, ra, id, r);
  zm = ode_adams_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100, va, iaad, raa, m,
                     rv, v, iad, ra, id, r);
  zm = ode_adams_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100, iaad, raa, m, rv, v,
                     iad, ra, id, r);
  zm = ode_adams_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100, raa, m, rv, v, iad,
                     ra, id, r);
  zm = ode_adams_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100, m, rv, v, iad, ra,
                     id, r);
  zm = ode_adams_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100, rv, v, iad, ra, id,
                     r);
  zm = ode_adams_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100, v, iad, ra, id, r);
  zm = ode_adams_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100, iad, ra, id, r);
  zm = ode_adams_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100, ra, id, r);
  zm = ode_adams_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100, id, r);
  zm = ode_adams_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100, r);
  zm = ode_adams_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
  
  r ~ normal(0, 1);
}
generated quantities {
  array[N] vector[N] zg = ode_adams_tol(f_12_arg, vd, rd, rad, 1e-6, 1e-6,
                                        100, mad, rvad, vad, iaad, raad, md,
                                        rvd, vd, iad, rad, id, rd);
  zg = ode_adams_tol(f_11_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvad, vad, iaad,
                     raad, md, rvd, vd, iad, rad, id, rd);
  zg = ode_adams_tol(f_10_arg, vd, rd, rad, 1e-6, 1e-6, 100, vad, iaad, raad,
                     md, rvd, vd, iad, rad, id, rd);
  zg = ode_adams_tol(f_9_arg, vd, rd, rad, 1e-6, 1e-6, 100, iaad, raad, md,
                     rvd, vd, iad, rad, id, rd);
  zg = ode_adams_tol(f_8_arg, vd, rd, rad, 1e-6, 1e-6, 100, raad, md, rvd,
                     vd, iad, rad, id, rd);
  zg = ode_adams_tol(f_7_arg, vd, rd, rad, 1e-6, 1e-6, 100, md, rvd, vd, iad,
                     rad, id, rd);
  zg = ode_adams_tol(f_6_arg, vd, rd, rad, 1e-6, 1e-6, 100, rvd, vd, iad,
                     rad, id, rd);
  zg = ode_adams_tol(f_5_arg, vd, rd, rad, 1e-6, 1e-6, 100, vd, iad, rad, id,
                     rd);
  zg = ode_adams_tol(f_4_arg, vd, rd, rad, 1e-6, 1e-6, 100, iad, rad, id, rd);
  zg = ode_adams_tol(f_3_arg, vd, rd, rad, 1e-6, 1e-6, 100, rad, id, rd);
  zg = ode_adams_tol(f_2_arg, vd, rd, rad, 1e-6, 1e-6, 100, id, rd);
  zg = ode_adams_tol(f_1_arg, vd, rd, rad, 1e-6, 1e-6, 100, rd);
  zg = ode_adams_tol(f_0_arg, vd, rd, rad, 1e-6, 1e-6, 100);
  
  zg = ode_adams_tol(f_12_arg, v, r, ra, 1e-6, 1e-6, 100, ma, rva, va, iaad,
                     raa, m, rv, v, iad, ra, id, r);
  zg = ode_adams_tol(f_11_arg, v, r, ra, 1e-6, 1e-6, 100, rva, va, iaad, raa,
                     m, rv, v, iad, ra, id, r);
  zg = ode_adams_tol(f_10_arg, v, r, ra, 1e-6, 1e-6, 100, va, iaad, raa, m,
                     rv, v, iad, ra, id, r);
  zg = ode_adams_tol(f_9_arg, v, r, ra, 1e-6, 1e-6, 100, iaad, raa, m, rv, v,
                     iad, ra, id, r);
  zg = ode_adams_tol(f_8_arg, v, r, ra, 1e-6, 1e-6, 100, raa, m, rv, v, iad,
                     ra, id, r);
  zg = ode_adams_tol(f_7_arg, v, r, ra, 1e-6, 1e-6, 100, m, rv, v, iad, ra,
                     id, r);
  zg = ode_adams_tol(f_6_arg, v, r, ra, 1e-6, 1e-6, 100, rv, v, iad, ra, id,
                     r);
  zg = ode_adams_tol(f_5_arg, v, r, ra, 1e-6, 1e-6, 100, v, iad, ra, id, r);
  zg = ode_adams_tol(f_4_arg, v, r, ra, 1e-6, 1e-6, 100, iad, ra, id, r);
  zg = ode_adams_tol(f_3_arg, v, r, ra, 1e-6, 1e-6, 100, ra, id, r);
  zg = ode_adams_tol(f_2_arg, v, r, ra, 1e-6, 1e-6, 100, id, r);
  zg = ode_adams_tol(f_1_arg, v, r, ra, 1e-6, 1e-6, 100, r);
  zg = ode_adams_tol(f_0_arg, v, r, ra, 1e-6, 1e-6, 100);
}

