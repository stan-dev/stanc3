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
  array[N] vector[N] zd = ode_adams(f_12_arg, vd, rd, rad, mad, rvad, vad,
                                    iaad, raad, md, rvd, vd, iad, rad, id,
                                    rd);
  zd = ode_adams(f_11_arg, vd, rd, rad, rvad, vad, iaad, raad, md, rvd, vd,
                 iad, rad, id, rd);
  zd = ode_adams(f_10_arg, vd, rd, rad, vad, iaad, raad, md, rvd, vd, iad,
                 rad, id, rd);
  zd = ode_adams(f_9_arg, vd, rd, rad, iaad, raad, md, rvd, vd, iad, rad, id,
                 rd);
  zd = ode_adams(f_8_arg, vd, rd, rad, raad, md, rvd, vd, iad, rad, id, rd);
  zd = ode_adams(f_7_arg, vd, rd, rad, md, rvd, vd, iad, rad, id, rd);
  zd = ode_adams(f_6_arg, vd, rd, rad, rvd, vd, iad, rad, id, rd);
  zd = ode_adams(f_5_arg, vd, rd, rad, vd, iad, rad, id, rd);
  zd = ode_adams(f_4_arg, vd, rd, rad, iad, rad, id, rd);
  zd = ode_adams(f_3_arg, vd, rd, rad, rad, id, rd);
  zd = ode_adams(f_2_arg, vd, rd, rad, id, rd);
  zd = ode_adams(f_1_arg, vd, rd, rad, rd);
  zd = ode_adams(f_0_arg, vd, rd, rad);
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
  array[N] vector[N] z = ode_adams(f_12_arg, vd, rd, rad, mad, rvad, vad,
                                   iaad, raad, md, rvd, vd, iad, rad, id, rd);
  z = ode_adams(f_11_arg, vd, rd, rad, rvad, vad, iaad, raad, md, rvd, vd,
                iad, rad, id, rd);
  z = ode_adams(f_10_arg, vd, rd, rad, vad, iaad, raad, md, rvd, vd, iad,
                rad, id, rd);
  z = ode_adams(f_9_arg, vd, rd, rad, iaad, raad, md, rvd, vd, iad, rad, id,
                rd);
  z = ode_adams(f_8_arg, vd, rd, rad, raad, md, rvd, vd, iad, rad, id, rd);
  z = ode_adams(f_7_arg, vd, rd, rad, md, rvd, vd, iad, rad, id, rd);
  z = ode_adams(f_6_arg, vd, rd, rad, rvd, vd, iad, rad, id, rd);
  z = ode_adams(f_5_arg, vd, rd, rad, vd, iad, rad, id, rd);
  z = ode_adams(f_4_arg, vd, rd, rad, iad, rad, id, rd);
  z = ode_adams(f_3_arg, vd, rd, rad, rad, id, rd);
  z = ode_adams(f_2_arg, vd, rd, rad, id, rd);
  z = ode_adams(f_1_arg, vd, rd, rad, rd);
  z = ode_adams(f_0_arg, vd, rd, rad);
  
  z = ode_adams(f_12_arg, v, r, ra, ma, rva, va, iaad, raa, m, rv, v, iad,
                ra, id, r);
  z = ode_adams(f_11_arg, v, r, ra, rva, va, iaad, raa, m, rv, v, iad, ra,
                id, r);
  z = ode_adams(f_10_arg, v, r, ra, va, iaad, raa, m, rv, v, iad, ra, id, r);
  z = ode_adams(f_9_arg, v, r, ra, iaad, raa, m, rv, v, iad, ra, id, r);
  z = ode_adams(f_8_arg, v, r, ra, raa, m, rv, v, iad, ra, id, r);
  z = ode_adams(f_7_arg, v, r, ra, m, rv, v, iad, ra, id, r);
  z = ode_adams(f_6_arg, v, r, ra, rv, v, iad, ra, id, r);
  z = ode_adams(f_5_arg, v, r, ra, v, iad, ra, id, r);
  z = ode_adams(f_4_arg, v, r, ra, iad, ra, id, r);
  z = ode_adams(f_3_arg, v, r, ra, ra, id, r);
  z = ode_adams(f_2_arg, v, r, ra, id, r);
  z = ode_adams(f_1_arg, v, r, ra, r);
  z = ode_adams(f_0_arg, v, r, ra);
}
model {
  array[N] vector[N] zm = ode_adams(f_12_arg, vd, rd, rad, mad, rvad, vad,
                                    iaad, raad, md, rvd, vd, iad, rad, id,
                                    rd);
  zm = ode_adams(f_11_arg, vd, rd, rad, rvad, vad, iaad, raad, md, rvd, vd,
                 iad, rad, id, rd);
  zm = ode_adams(f_10_arg, vd, rd, rad, vad, iaad, raad, md, rvd, vd, iad,
                 rad, id, rd);
  zm = ode_adams(f_9_arg, vd, rd, rad, iaad, raad, md, rvd, vd, iad, rad, id,
                 rd);
  zm = ode_adams(f_8_arg, vd, rd, rad, raad, md, rvd, vd, iad, rad, id, rd);
  zm = ode_adams(f_7_arg, vd, rd, rad, md, rvd, vd, iad, rad, id, rd);
  zm = ode_adams(f_6_arg, vd, rd, rad, rvd, vd, iad, rad, id, rd);
  zm = ode_adams(f_5_arg, vd, rd, rad, vd, iad, rad, id, rd);
  zm = ode_adams(f_4_arg, vd, rd, rad, iad, rad, id, rd);
  zm = ode_adams(f_3_arg, vd, rd, rad, rad, id, rd);
  zm = ode_adams(f_2_arg, vd, rd, rad, id, rd);
  zm = ode_adams(f_1_arg, vd, rd, rad, rd);
  zm = ode_adams(f_0_arg, vd, rd, rad);
  
  zm = ode_adams(f_12_arg, v, r, ra, ma, rva, va, iaad, raa, m, rv, v, iad,
                 ra, id, r);
  zm = ode_adams(f_11_arg, v, r, ra, rva, va, iaad, raa, m, rv, v, iad, ra,
                 id, r);
  zm = ode_adams(f_10_arg, v, r, ra, va, iaad, raa, m, rv, v, iad, ra, id, r);
  zm = ode_adams(f_9_arg, v, r, ra, iaad, raa, m, rv, v, iad, ra, id, r);
  zm = ode_adams(f_8_arg, v, r, ra, raa, m, rv, v, iad, ra, id, r);
  zm = ode_adams(f_7_arg, v, r, ra, m, rv, v, iad, ra, id, r);
  zm = ode_adams(f_6_arg, v, r, ra, rv, v, iad, ra, id, r);
  zm = ode_adams(f_5_arg, v, r, ra, v, iad, ra, id, r);
  zm = ode_adams(f_4_arg, v, r, ra, iad, ra, id, r);
  zm = ode_adams(f_3_arg, v, r, ra, ra, id, r);
  zm = ode_adams(f_2_arg, v, r, ra, id, r);
  zm = ode_adams(f_1_arg, v, r, ra, r);
  zm = ode_adams(f_0_arg, v, r, ra);
  
  r ~ normal(0, 1);
}
generated quantities {
  array[N] vector[N] zg = ode_adams(f_12_arg, vd, rd, rad, mad, rvad, vad,
                                    iaad, raad, md, rvd, vd, iad, rad, id,
                                    rd);
  zg = ode_adams(f_11_arg, vd, rd, rad, rvad, vad, iaad, raad, md, rvd, vd,
                 iad, rad, id, rd);
  zg = ode_adams(f_10_arg, vd, rd, rad, vad, iaad, raad, md, rvd, vd, iad,
                 rad, id, rd);
  zg = ode_adams(f_9_arg, vd, rd, rad, iaad, raad, md, rvd, vd, iad, rad, id,
                 rd);
  zg = ode_adams(f_8_arg, vd, rd, rad, raad, md, rvd, vd, iad, rad, id, rd);
  zg = ode_adams(f_7_arg, vd, rd, rad, md, rvd, vd, iad, rad, id, rd);
  zg = ode_adams(f_6_arg, vd, rd, rad, rvd, vd, iad, rad, id, rd);
  zg = ode_adams(f_5_arg, vd, rd, rad, vd, iad, rad, id, rd);
  zg = ode_adams(f_4_arg, vd, rd, rad, iad, rad, id, rd);
  zg = ode_adams(f_3_arg, vd, rd, rad, rad, id, rd);
  zg = ode_adams(f_2_arg, vd, rd, rad, id, rd);
  zg = ode_adams(f_1_arg, vd, rd, rad, rd);
  zg = ode_adams(f_0_arg, vd, rd, rad);
  
  zg = ode_adams(f_12_arg, v, r, ra, ma, rva, va, iaad, raa, m, rv, v, iad,
                 ra, id, r);
  zg = ode_adams(f_11_arg, v, r, ra, rva, va, iaad, raa, m, rv, v, iad, ra,
                 id, r);
  zg = ode_adams(f_10_arg, v, r, ra, va, iaad, raa, m, rv, v, iad, ra, id, r);
  zg = ode_adams(f_9_arg, v, r, ra, iaad, raa, m, rv, v, iad, ra, id, r);
  zg = ode_adams(f_8_arg, v, r, ra, raa, m, rv, v, iad, ra, id, r);
  zg = ode_adams(f_7_arg, v, r, ra, m, rv, v, iad, ra, id, r);
  zg = ode_adams(f_6_arg, v, r, ra, rv, v, iad, ra, id, r);
  zg = ode_adams(f_5_arg, v, r, ra, v, iad, ra, id, r);
  zg = ode_adams(f_4_arg, v, r, ra, iad, ra, id, r);
  zg = ode_adams(f_3_arg, v, r, ra, ra, id, r);
  zg = ode_adams(f_2_arg, v, r, ra, id, r);
  zg = ode_adams(f_1_arg, v, r, ra, r);
  zg = ode_adams(f_0_arg, v, r, ra);
}

