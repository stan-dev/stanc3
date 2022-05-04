functions {
  vector single_ret_fun(vector A) {
    return A;
  }
  vector multi_ret_fun(vector A) {
    vector[5] B;
    if (A[0] > 1.0) {
      return B;
    } else {
      return A;
    }
  }
}

parameters {
 vector[5] p_single_ret_vec;
 vector[5] p_multi_ret_vec;
}

transformed parameters {
  vector[5] tp_single_ret_vec = single_ret_fun(p_single_ret_vec);
  vector[5] tp_multi_ret_vec = multi_ret_fun(p_multi_ret_vec);
} 