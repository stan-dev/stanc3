functions {
    matrix okay_reduction(real sum_x, matrix y) {
        return sum_x + y;
    }
}

data {
    real data_r;
}

parameters {
    matrix [5, 10] soa_x;
    matrix [5, 10] aos_x;
    matrix [5, 10] should_not_be_but_is_aos_x;
    matrix [5, 10] aos_y;
}

transformed parameters {
    real tp_real_from_soa = 5 > 0 ? data_r : sum(soa_x);
    matrix[5, 10] tp_matrix_aos_from_mix = sum(should_not_be_but_is_aos_x) > 0 ? aos_x : aos_y;
    matrix[5, 10] tp_matrix_from_udf_reduced_soa = okay_reduction(sum(soa_x), aos_x);
}