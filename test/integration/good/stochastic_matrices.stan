data {
   stochastic_column_matrix[10, 10] d_scm;
   stochastic_row_matrix[10, 10] d_srm;
}

transformed data {
   stochastic_column_matrix[10, 10] td_scm = d_scm;
   stochastic_row_matrix[10, 10] td_srm = d_srm;
}
parameters {
   stochastic_column_matrix[10, 10] p_scm;
   stochastic_row_matrix[10, 10] p_srm;
}

transformed parameters {
   stochastic_column_matrix[10, 10] tp_scm = p_scm;
   stochastic_row_matrix[10, 10] tp_srm = p_srm;
}

generated quantities {
   stochastic_column_matrix[10, 10] gq_scm = tp_scm;
   stochastic_row_matrix[10, 10] gq_srm = tp_srm;
}