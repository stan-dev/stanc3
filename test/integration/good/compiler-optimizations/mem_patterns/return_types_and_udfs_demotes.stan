/*
 * Testing that 
 * 1. User defined funcs demote
 * 2. Empty funcs demote 
 * 3. Funcs with no eigen matrices on rhs demote 
 * 4. UDFs demote
 * 4. functions returning a non-matrix type and a rhs correctly demote
 */
functions {
    matrix empty_user_func() {
        matrix[10, 10] some_mat;
        return some_mat;
    }
    matrix mat_ret_user_func(matrix A) {
        return A;
    }
}

parameters {
    matrix[10, 10] row_soa;
    matrix[10, 10] udf_input_aos;
}

transformed parameters {
    matrix[10, 10] user_func_aos = mat_ret_user_func(udf_input_aos);
    matrix[10, 10] empty_user_func_aos = empty_user_func();
    matrix[10, 10] inner_empty_user_func_aos = multiply(udf_input_aos, empty_user_func());
    matrix[10, 10] int_aos_mul_aos = rows(row_soa) * empty_user_func_aos;
    matrix[10, 10] mul_two_aos = cols(row_soa) * transpose(int_aos_mul_aos) * user_func_aos;
}