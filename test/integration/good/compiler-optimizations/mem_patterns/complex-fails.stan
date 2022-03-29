parameters {
    matrix[10, 10] A_p;
}

transformed parameters {
   complex_matrix[10, 10] A_complex_tp = A_p;
}