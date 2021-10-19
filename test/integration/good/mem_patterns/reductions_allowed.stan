functions {
    matrix nono_func(matrix x) {
        return x;
    }

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
    matrix [5, 10] aos_y;
}

transformed parameters {
    real tp_real_from_soa = 5 > 0 ? data_r : sum(soa_x);
    matrix[5, 10] tp_matrix_aos_from_mix = sum(soa_x) > 0 ? nono_func(aos_x + aos_y) : aos_y;
    matrix[5, 10] tp_matrix_from_udf_reduced_soa = okay_reduction(sum(soa_x), aos_x);
}