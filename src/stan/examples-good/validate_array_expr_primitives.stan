functions {
  int[] f1_arr_int() {
    return  {-1, -2};
  }
  int[] f2_arr_int(int x) {
    return  {-x};
  }
  real[] f1_arr_real() {
    return  {-1.0, -2.0};
  }
  real[] f2_arr_real(real y) {
    return  {-y};
  }
}
data { 
  int d_i1;
  int d_i2;
  int d_i3;
  real d_r1;
  real d_r2;
  real d_r3;
} 
transformed data {
  int td_arr_int_d1_1[3] = {1, 2, 3};
  int td_arr_int_d1_2[3] = {d_i1, 2, 3};
  int td_arr_int_d1_3[2] = f1_arr_int();
  int td_arr_int_d1_4[1] = f2_arr_int(d_i2);
  int td_arr_int_d2_1[1,2] = {{4, 5}};
  int td_arr_int_d2_2[1,2] = {{4, d_i3}};
  int td_arr_int_d2_3[2,3] = {{1,2,3},{4,5,6}};
  int td_arr_int_d2_4[2,3] = { td_arr_int_d1_1, td_arr_int_d1_2 };

  real td_arr_real_d1_1[3] = {1.1, 2.2, 3.3};
  real td_arr_real_d1_2[3] = {d_r1, 2, 3};

}
