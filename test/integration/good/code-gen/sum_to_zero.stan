data {
   sum_to_zero_vector[10] d_stzv;
   array[2, 3] sum_to_zero_vector[10] d_astzv;
   sum_to_zero_matrix[4,5] d_stzm;
   array[2, 3] sum_to_zero_matrix[4,5] d_astzm;
}

transformed data {
   int v = 10;
   int N = 4;
   sum_to_zero_vector[10] td_stzv = d_stzv;
   array[2, 3] sum_to_zero_vector[10] td_astzv;
   sum_to_zero_matrix[4,5] td_stzm = d_stzm;
   array[2, 3] sum_to_zero_matrix[4,5] td_astzm;
}
parameters {
   sum_to_zero_vector[v] p_stzv;
   array[2, 3] sum_to_zero_vector[v] p_astzv;
   sum_to_zero_matrix[N,N+1] p_stzm;
   array[2, 3] sum_to_zero_matrix[N,N+1] p_astzm;
}

transformed parameters {
   sum_to_zero_vector[10] tp_stzv = p_stzv;
   array[2, 3] sum_to_zero_vector[10] tp_astzv = p_astzv;
   sum_to_zero_matrix[4,5] tp_stzm = p_stzm;
   array[2, 3] sum_to_zero_matrix[4,5] tp_astzm = p_astzm;
}

generated quantities {
   sum_to_zero_vector[10] gq_stzv = tp_stzv;
   array[2, 3] sum_to_zero_vector[10] gq_astzv = tp_astzv;
   sum_to_zero_matrix[4,5] gq_stzm = tp_stzm;
   array[2, 3] sum_to_zero_matrix[4,5] gq_astzm = tp_astzm;
}
