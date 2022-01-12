/**
 * Tests Single index passing and failing for in and out of loops
 */

 parameters {
   matrix[10, 10] aos_p;
   matrix[10, 10] soa_p;
 }

 transformed parameters {
     real tp_real_from_soa = soa_p[1, 1];
     for (i in 1:10) {
       // Okay because of row index
       row_vector[10] tp_row_vector_from_soa_loop = multiply(soa_p[i], soa_p);
       // lhs is soa but aos_p is demoted here
       matrix[10, 10] tp_matrix_from_soa_loop = multiply(aos_p[i, i], soa_p);
     }
 }