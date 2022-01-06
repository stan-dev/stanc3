functions {
    matrix user_func() {
        matrix[10, 10] some_mat;
        return some_mat;
    }
}

parameters {
    matrix[10, 10] b_soa;
}

transformed parameters {
    matrix[10, 10] c_aos = user_func();
    matrix[10, 10] d_aos = user_func();
    matrix[10, 10] cond_aos = rows(b_soa) > 1 ? c_aos : d_aos;
    matrix[10, 10] int_aos_mul_aos = rows(b_soa) * c_aos;
}