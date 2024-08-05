data {
   column_stochastic_matrix[10, 10] d_csm;
   row_stochastic_matrix[10, 10] d_rsm;
   array[2, 2] row_stochastic_matrix[10, 10] d_arsm;
}

transformed data {
   column_stochastic_matrix[10, 10] td_csm = d_csm;
   row_stochastic_matrix[10, 10] td_rsm = d_rsm;
   array[2, 2] row_stochastic_matrix[10, 10] td_arsm;
}
parameters {
   column_stochastic_matrix[10, 10] p_csm;
   row_stochastic_matrix[10, 10] p_rsm;
   array[2, 2] row_stochastic_matrix[10, 10] p_arsm;
}

transformed parameters {
   column_stochastic_matrix[10, 10] tp_csm = p_csm;
   row_stochastic_matrix[10, 10] tp_rsm = p_rsm;
   array[2, 2] row_stochastic_matrix[10, 10] tp_arsm = p_arsm;
}

generated quantities {
   column_stochastic_matrix[10, 10] gq_csm = tp_csm;
   row_stochastic_matrix[10, 10] gq_rsm = tp_rsm;
   array[2, 2] row_stochastic_matrix[10, 10] gq_arsm = tp_arsm;
}
