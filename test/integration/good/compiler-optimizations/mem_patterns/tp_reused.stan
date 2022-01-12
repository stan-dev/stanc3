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
    matrix [5, 10] first_pass_soa_x;
    matrix [5, 10] aos_x;
}

transformed parameters {
    matrix[5, 10] tp_matrix_aos = first_pass_soa_x;
    tp_matrix_aos = nono_func(aos_x);
}