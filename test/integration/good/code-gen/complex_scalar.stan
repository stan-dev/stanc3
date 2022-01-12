functions {
  complex foo() {
    return to_complex();
  }
  real foo1(complex z) {
    return 1.0;
  }
  complex foo2(real r) {
    return to_complex(r);
  }
  complex foo3(complex z) {
    return z;
  }
  array[] complex foo4() {
    return {to_complex(), to_complex()};
  }
  real foo5(array[] complex z) {
    return 1.0;
  }
  array[] complex foo6(real r) {
    return {to_complex(r), to_complex(r, r)};
  }
  array[] complex foo7(array[] complex z) {
    return z;
  }
  real foo8(array[,] complex z) {
    return 1.0;
  }
  array[,] complex foo9(real r) {
    return {{to_complex(r), to_complex(r), to_complex(r)},
            {to_complex(r), to_complex(r), to_complex(r)}};
  }
  array[,] complex foo10(array[,] complex z) {
    return z;
  }
}
data {
  int d_i;
  real d_r;
  complex d_complex;
  array[2] complex d_complex_array;
  array[2, 3] complex d_complex_array_2d;
}
transformed data {
  int td_i = 1;
  real td_r = 1.1;
  complex td_complex;
  array[2] complex td_complex_array;
  array[2, 3] complex td_complex_array_2d;

  td_complex = td_i;
  td_complex = td_r;
  td_complex = d_i;
  td_complex = d_r;
  td_complex = 1;
  td_complex = 1.1;
  td_complex = d_complex;
  td_complex = d_complex_array[1];
  td_complex = to_complex();
  td_complex = to_complex(1);
  td_complex = to_complex(1.2);
  td_complex = to_complex(1, 2);
  td_complex = to_complex(1.1, 2);
  td_complex = to_complex(1, 2.2);
  td_complex = to_complex(1.1, 2.2);

  td_complex_array = d_complex_array;
  td_complex_array = {td_complex, 1, to_complex(2, 3)};
  td_complex_array[1] = to_complex(5.1, 6);

  td_complex_array_2d = d_complex_array_2d;
  td_complex_array_2d = {{1, td_complex, 3},
                         {to_complex(), to_complex(1.1), to_complex(1, 2.1)}};
  td_complex_array_2d[1, 1] = to_complex(1, 2);
  td_complex_array_2d[1, 1] = 1;
  td_complex_array_2d[1] = td_complex_array;
  td_complex_array_2d[1] = {td_complex, to_complex(1, 2), 2.4};

  for (td_j in 1 : 2) {
    for (td_k in 1 : 3) {
      td_complex_array_2d[td_j, td_k] = to_complex(1, 2.2);
    }
  }

  for (td_j in td_complex_array_2d) {
    for (td_k in td_j) {
      td_complex = td_k;
    }
  }

  td_r = get_real(td_complex);
  td_r = get_imag(td_complex);
  td_r = get_real(td_complex_array[1]);
  td_r = get_imag(td_complex_array[1]);
  td_r = get_real(td_complex_array_2d[1, 1]);
  td_r = get_imag(td_complex_array_2d[1, 1]);

  td_complex = foo();
  td_r = foo1(td_complex);
  td_complex = foo2(td_r);
  td_complex = foo3(td_complex);
  td_complex_array = foo4();
  td_r = foo5(td_complex_array);
  td_complex_array = foo6(td_r);
  td_complex_array = foo7(td_complex_array);
  td_r = foo8(td_complex_array_2d);
  td_complex_array_2d = foo9(td_r);
  td_complex_array_2d = foo10(td_complex_array_2d);
}
parameters {
  real p_r;
  complex p_complex;
  array[2] complex p_complex_array;
  array[2, 3] complex p_complex_array_2d;
}
transformed parameters {
  real tp_r = 1.1;
  complex tp_complex;
  array[2] complex tp_complex_array;
  array[2, 3] complex tp_complex_array_2d;

  tp_complex = tp_r;
  tp_complex = d_i;
  tp_complex = d_r;
  tp_complex = p_r;
  tp_complex = 1;
  tp_complex = 1.1;
  tp_complex = d_complex;
  tp_complex = d_complex_array[1];
  tp_complex = p_complex;
  tp_complex = p_complex_array[1];
  tp_complex = to_complex();
  tp_complex = to_complex(1);
  tp_complex = to_complex(1.2);
  tp_complex = to_complex(1, 2);
  tp_complex = to_complex(1.1, 2);
  tp_complex = to_complex(1, 2.2);
  tp_complex = to_complex(1.1, 2.2);

  tp_complex_array = d_complex_array;
  tp_complex_array = p_complex_array;
  tp_complex_array = {tp_complex, 1, to_complex(2, 3)};
  tp_complex_array[1] = to_complex(5.1, 6);

  tp_complex_array_2d = d_complex_array_2d;
  tp_complex_array_2d = p_complex_array_2d;
  tp_complex_array_2d = {{1, tp_complex, 3},
                         {to_complex(), to_complex(1.1), to_complex(1, 2.1)}};
  tp_complex_array_2d[1, 1] = to_complex(1, 2);
  tp_complex_array_2d[1, 1] = 1;
  tp_complex_array_2d[1] = tp_complex_array;
  tp_complex_array_2d[1] = {tp_complex, to_complex(1, 2), 2.4};

  for (tp_j in 1 : 2) {
    for (tp_k in 1 : 3) {
      tp_complex_array_2d[tp_j, tp_k] = to_complex(1, 2.2);
    }
  }

  for (tp_j in tp_complex_array_2d) {
    for (tp_k in tp_j) {
      tp_complex = tp_k;
    }
  }

  tp_r = get_real(tp_complex);
  tp_r = get_imag(tp_complex);
  tp_r = get_real(tp_complex_array[1]);
  tp_r = get_imag(tp_complex_array[1]);
  tp_r = get_real(tp_complex_array_2d[1, 1]);
  tp_r = get_imag(tp_complex_array_2d[1, 1]);

  tp_complex = foo();
  tp_r = foo1(tp_complex);
  tp_complex = foo2(tp_r);
  tp_complex = foo3(tp_complex);
  tp_complex_array = foo4();
  tp_r = foo5(tp_complex_array);
  tp_complex_array = foo6(tp_r);
  tp_complex_array = foo7(tp_complex_array);
  tp_r = foo8(tp_complex_array_2d);
  tp_complex_array_2d = foo9(tp_r);
  tp_complex_array_2d = foo10(tp_complex_array_2d);
}
model {
  complex m_complex;
  array[2] complex m_complex_array;
  array[2, 3] complex m_complex_array_2d;

  abs(p_complex) ~ normal(0, 1);
}
generated quantities {
  int gq_i = 1;
  real gq_r = 1.1;
  complex gq_complex;
  array[2] complex gq_complex_array;
  array[2, 3] complex gq_complex_array_2d;

  gq_complex = gq_i;
  gq_complex = gq_r;
  gq_complex = d_i;
  gq_complex = d_r;
  gq_complex = p_r;
  gq_complex = 1;
  gq_complex = 1.1;
  gq_complex = d_complex;
  gq_complex = d_complex_array[1];
  gq_complex = p_complex;
  gq_complex = p_complex_array[1];
  gq_complex = to_complex();
  gq_complex = to_complex(1);
  gq_complex = to_complex(1.2);
  gq_complex = to_complex(1, 2);
  gq_complex = to_complex(1.1, 2);
  gq_complex = to_complex(1, 2.2);
  gq_complex = to_complex(1.1, 2.2);

  gq_complex_array = d_complex_array;
  gq_complex_array = p_complex_array;
  gq_complex_array = {gq_complex, 1, to_complex(2, 3)};
  gq_complex_array[1] = to_complex(5.1, 6);

  gq_complex_array_2d = d_complex_array_2d;
  gq_complex_array_2d = p_complex_array_2d;
  gq_complex_array_2d = {{1, gq_complex, 3},
                         {to_complex(), to_complex(1.1), to_complex(1, 2.1)}};
  gq_complex_array_2d[1, 1] = to_complex(1, 2);
  gq_complex_array_2d[1, 1] = 1;
  gq_complex_array_2d[1] = gq_complex_array;
  gq_complex_array_2d[1] = {gq_complex, to_complex(1, 2), 2.4};

  for (gq_j in 1 : 2) {
    for (gq_k in 1 : 3) {
      gq_complex_array_2d[gq_j, gq_k] = to_complex(1, 2.2);
    }
  }

  for (gq_j in gq_complex_array_2d) {
    for (gq_k in gq_j) {
      gq_complex = gq_k;
    }
  }

  gq_r = get_real(gq_complex);
  gq_r = get_imag(gq_complex);
  gq_r = get_real(gq_complex_array[1]);
  gq_r = get_imag(gq_complex_array[1]);
  gq_r = get_real(gq_complex_array_2d[1, 1]);
  gq_r = get_imag(gq_complex_array_2d[1, 1]);

  gq_complex = foo();
  gq_r = foo1(gq_complex);
  gq_complex = foo2(gq_r);
  gq_complex = foo3(gq_complex);
  gq_complex_array = foo4();
  gq_r = foo5(gq_complex_array);
  gq_complex_array = foo6(gq_r);
  gq_complex_array = foo7(gq_complex_array);
  gq_r = foo8(gq_complex_array_2d);
  gq_complex_array_2d = foo9(gq_r);
  gq_complex_array_2d = foo10(gq_complex_array_2d);

  complex z = to_complex(1, 2);
  complex y = to_complex(3, 4);
  array[0] int i_arr;
  array[1] int i_arr_1;

  gq_complex = z + y;
  gq_complex = z + gq_r;
  gq_complex = gq_r + z;
  gq_complex = z + gq_i;
  gq_complex = gq_i + z;
  gq_complex = d_complex + p_complex;
  gq_complex = d_complex + d_r;
  gq_complex = d_complex + p_r;
  gq_complex = d_r + p_complex;
  gq_complex = p_complex + p_r;
  gq_complex = d_complex + gq_i;
  gq_complex = gq_i + p_complex;

  gq_complex = z - y;
  gq_complex = z - gq_r;
  gq_complex = gq_r - z;
  gq_complex = z - gq_i;
  gq_complex = gq_i - z;
  gq_complex = d_complex - p_complex;
  gq_complex = d_complex - d_r;
  gq_complex = d_complex - p_r;
  gq_complex = d_r - p_complex;
  gq_complex = p_complex - p_r;
  gq_complex = d_complex - gq_i;
  gq_complex = gq_i - p_complex;

  gq_complex = z * y;
  gq_complex = z * gq_r;
  gq_complex = gq_r * z;
  gq_complex = z * gq_i;
  gq_complex = gq_i * z;
  gq_complex = d_complex * p_complex;
  gq_complex = d_complex * d_r;
  gq_complex = d_complex * p_r;
  gq_complex = d_r * p_complex;
  gq_complex = p_complex * p_r;
  gq_complex = d_complex * gq_i;
  gq_complex = gq_i * p_complex;

  gq_complex = z / y;
  gq_complex = z / gq_r;
  gq_complex = gq_r / z;
  gq_complex = z / gq_i;
  gq_complex = gq_i / z;
  gq_complex = d_complex / p_complex;
  gq_complex = d_complex / d_r;
  gq_complex = d_complex / p_r;
  gq_complex = d_r / p_complex;
  gq_complex = p_complex / p_r;
  gq_complex = d_complex / gq_i;
  gq_complex = gq_i / p_complex;

  gq_complex = z ^ y;
  gq_complex = z ^ gq_r;
  gq_complex = gq_r ^ z;
  gq_complex = z ^ gq_i;
  gq_complex = gq_i ^ z;
  gq_complex = d_complex ^ p_complex;
  gq_complex = d_complex ^ d_r;
  gq_complex = d_complex ^ p_r;
  gq_complex = d_r ^ p_complex;
  gq_complex = p_complex ^ p_r;
  gq_complex = d_complex ^ gq_i;
  gq_complex = gq_i ^ p_complex;

  gq_complex = -z;
  gq_complex = -gq_r;
  gq_complex = -gq_i;
  gq_complex = -d_complex;
  gq_complex = -d_r;
  gq_complex = -p_complex;
  gq_complex = -p_r;

  gq_complex = gq_i ? z : y;
  gq_complex = gq_i ? p_complex : z;
  gq_complex = gq_i ? d_complex : z;

  gq_i = (z == z);
  gq_i = (z == gq_r);
  gq_i = (z == gq_i);
  gq_i = (p_complex == z);
  gq_i = (d_complex == z);
  gq_i = (p_complex == d_complex);
  gq_i = (p_complex == p_r);
  gq_i = (d_complex == d_r);
  gq_i = (p_r == d_complex);
  gq_i = (p_complex == d_r);

  gq_i = (z != z);
  gq_i = (z != gq_r);
  gq_i = (z != gq_i);
  gq_i = (p_complex != z);
  gq_i = (d_complex != z);
  gq_i = (p_complex != d_complex);
  gq_i = (p_complex != p_r);
  gq_i = (d_complex != d_r);
  gq_i = (p_r != d_complex);
  gq_i = (p_complex != d_r);

  gq_r = abs(z);
  gq_r = abs(p_complex);
  gq_r = abs(d_complex);

  gq_complex = acos(z);
  gq_complex = acos(p_complex);
  gq_complex = acos(d_complex);

  gq_complex = acosh(z);
  gq_complex = acosh(p_complex);
  gq_complex = acosh(d_complex);

  gq_complex = asin(z);
  gq_complex = asin(p_complex);
  gq_complex = asin(d_complex);

  gq_complex = asinh(z);
  gq_complex = asinh(p_complex);
  gq_complex = asinh(d_complex);

  gq_complex = atan(z);
  gq_complex = atan(p_complex);
  gq_complex = atan(d_complex);

  gq_complex = atanh(z);
  gq_complex = atanh(p_complex);
  gq_complex = atanh(d_complex);

  gq_complex = conj(z);
  gq_complex = conj(p_complex);
  gq_complex = conj(d_complex);

  gq_complex = cos(z);
  gq_complex = cos(p_complex);
  gq_complex = cos(d_complex);

  gq_complex = cosh(z);
  gq_complex = cosh(p_complex);
  gq_complex = cosh(d_complex);

  i_arr = dims(z);
  i_arr = dims(p_complex);
  i_arr = dims(d_complex);
  i_arr_1 = dims(gq_complex_array);
  i_arr_1 = dims(p_complex_array);
  i_arr_1 = dims(d_complex_array);

  gq_complex = exp(z);
  gq_complex = exp(p_complex);
  gq_complex = exp(d_complex);

  gq_r = get_imag(z);
  gq_r = get_imag(p_complex);
  gq_r = get_imag(d_complex);

  gq_r = get_real(z);
  gq_r = get_real(p_complex);
  gq_r = get_real(d_complex);

  gq_complex_array = head(gq_complex_array, 1);
  gq_complex_array = head(p_complex_array, 1);
  gq_complex_array = head(d_complex_array, 1);

  gq_complex = log(z);
  gq_complex = log(p_complex);
  gq_complex = log(d_complex);

  gq_complex = log10(z);
  gq_complex = log10(p_complex);
  gq_complex = log10(d_complex);

  gq_r = norm(z);
  gq_r = norm(p_complex);
  gq_r = norm(d_complex);

  gq_i = num_elements(gq_complex_array);
  gq_i = num_elements(p_complex_array);
  gq_i = num_elements(d_complex_array);

  gq_complex = polar(gq_r, gq_r);
  gq_complex = polar(gq_i, gq_i);
  gq_complex = polar(gq_r, gq_i);
  gq_complex = polar(gq_i, gq_r);
  gq_complex = polar(p_r, gq_r);
  gq_complex = polar(d_r, gq_r);
  gq_complex = polar(p_r, d_r);

  gq_complex = pow(z, gq_r);
  gq_complex = pow(z, gq_i);
  gq_complex = pow(z, z);
  gq_complex = pow(p_complex, p_r);
  gq_complex = pow(p_complex, d_r);
  gq_complex = pow(p_complex, gq_r);
  gq_complex = pow(p_complex, z);
  gq_complex = pow(p_complex, d_complex);
  gq_complex = pow(p_complex, p_complex);
  gq_complex = pow(d_complex, p_r);
  gq_complex = pow(d_complex, d_r);
  gq_complex = pow(d_complex, gq_r);
  gq_complex = pow(d_complex, z);
  gq_complex = pow(d_complex, d_complex);
  gq_complex = pow(d_complex, p_complex);

  gq_complex = proj(z);
  gq_complex = proj(p_complex);
  gq_complex = proj(d_complex);

  gq_complex_array = reverse(gq_complex_array);
  gq_complex_array = reverse(p_complex_array);
  gq_complex_array = reverse(d_complex_array);

  gq_complex = sin(z);
  gq_complex = sin(p_complex);
  gq_complex = sin(d_complex);

  gq_complex = sinh(z);
  gq_complex = sinh(p_complex);
  gq_complex = sinh(d_complex);

  gq_i = size(gq_complex_array);
  gq_i = size(p_complex_array);
  gq_i = size(d_complex_array);

  gq_complex = sqrt(z);
  gq_complex = sqrt(p_complex);
  gq_complex = sqrt(d_complex);

  gq_complex_array = tail(gq_complex_array, 1);
  gq_complex_array = tail(p_complex_array, 1);
  gq_complex_array = tail(d_complex_array, 1);

  gq_complex = tan(z);
  gq_complex = tan(p_complex);
  gq_complex = tan(d_complex);

  gq_complex = tanh(z);
  gq_complex = tanh(p_complex);
  gq_complex = tanh(d_complex);

  gq_complex = to_complex();
  gq_complex = to_complex(gq_r, gq_r);
  gq_complex = to_complex(gq_i, gq_i);
  gq_complex = to_complex(gq_r, gq_i);
  gq_complex = to_complex(gq_i, gq_r);
  gq_complex = to_complex(gq_r);
  gq_complex = to_complex(gq_i);
  gq_complex = to_complex(p_r, gq_r);
  gq_complex = to_complex(p_r, d_r);
  gq_complex = to_complex(d_r, gq_r);
  gq_complex = to_complex(p_r);
  gq_complex = to_complex(d_r);

  // test imaginary literal
  complex zi = 1 + 3.14i;
  zi = zi * 0i;
  complex yi = to_complex(0, 1.1) + to_complex(0.0, 2.2) + to_complex();
  real x = get_real(3i - 40e-3i);
}
