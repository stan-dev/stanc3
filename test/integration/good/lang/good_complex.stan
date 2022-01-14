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
}
data {
  int<lower=0> N;
  complex d_complex;
  array[N] complex d_complex_array;
  array[N, 2] complex d_complex_array_2d;
  array[N, 2, 3] complex d_complex_array_3d;
}
transformed data {
  complex td_complex = d_complex;
  td_complex = 1;
  td_complex = to_complex(1, 2);
  array[N] complex td_complex_array = d_complex_array;
  array[2, 3] complex td_complex_array_2d;
  array[2, 3, 4] complex td_complex_array_3d;
  for (td_i in 1 : 2) {
    for (td_j in 1 : 3) {
      for (td_k in 1 : 4) {
        td_complex_array_3d[td_i, td_j, td_k] = to_complex(1, 2.2);
      }
    }
  }
  real td_r = get_real(td_complex);
  real td_i = get_imag(td_complex);
}
parameters {
  complex p_complex;
  array[N] complex p_complex_array;
  array[N, 2] complex p_complex_array_2d;
  array[N, 2, 3] complex p_complex_array_3d;
}
transformed parameters {
  complex tp_complex = p_complex;
  tp_complex = 1;
  tp_complex = to_complex(1, 2);
  array[N] complex tp_complex_array = p_complex_array;
  array[2, 3] complex tp_complex_array_2d;
  array[2, 3, 4] complex tp_complex_array_3d;
  for (tp_i in 1 : 2) {
    for (tp_j in 1 : 3) {
      for (tp_k in 1 : 4) {
        tp_complex_array_3d[tp_i, tp_j, tp_k] = to_complex(1, 2.2);
      }
    }
  }
  real tp_r = get_real(tp_complex);
  real tp_i = get_imag(tp_complex);
}
model {
  abs(p_complex) ~ normal(0, 1);
}
generated quantities {
  complex gq_complex;
  array[2] complex gq_complex_array;
  array[2, 3] complex gq_complex_array_2d = {{1, 2, 3},
                                             {to_complex(), to_complex(
                                              1.1), to_complex(1, 2.1)}};
  array[2, 3, 4] complex gq_complex_array_3d;
  gq_complex = 3;
  gq_complex_array[1] = to_complex(5.1, 6);
  gq_complex = foo();
  real gq_r = foo1(gq_complex);
  gq_complex = foo2(gq_r);
  gq_complex = foo3(gq_complex);
  gq_complex_array = foo4();
  gq_r = foo5(gq_complex_array);
  gq_complex_array = foo6(gq_r);
  gq_complex_array = foo7(gq_complex_array);

  // test imaginary literal
  complex zi = 1 + 3.14i;
  zi = zi * 0i;
  complex yi = to_complex(0, 1.1) + to_complex(0.0, 2.2) + to_complex();
  real x = get_real(3i - 40e-3i);
}
