data {
   sum_to_zero[10] d_stz;
   array[2, 3] sum_to_zero[10] d_astz;
}

transformed data {
   sum_to_zero[10] td_stz = d_stz;
   array[2, 3] sum_to_zero[10] td_astz;
}
parameters {
   sum_to_zero[10] p_stz;
   array[2, 3] sum_to_zero[10] p_astz;
}

transformed parameters {
   sum_to_zero[10] tp_stz = p_stz;
   array[2, 3] sum_to_zero[10] tp_astz = p_astz;
}

generated quantities {
   sum_to_zero[10] gq_stz = tp_stz;
   array[2, 3] sum_to_zero[10] gq_astz = tp_astz;
}
