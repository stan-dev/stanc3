functions {
   complex foo(complex x){
       return x;
   }
}
data {
    int<lower = 0> N;
    complex d_complex;
    complex d_complex_array[N];
    complex d_complex_array_2d[N, 2];
    complex d_complex_array_3d[N, 2, 3];
}
transformed data {
    complex td_complex = d_complex;
    td_complex = 1;
    td_complex = to_complex(1,2);
    complex td_complex_array[N] = d_complex_array;
    complex td_complex_array_2d[2, 3];
    complex td_complex_array_3d[2, 3, 4];
    for (td_i in 1:2) {
        for (td_j in 1:3) {
            for (td_k in 1:4) {
                td_complex_array_3d[td_i, td_j, td_k] = to_complex(1, 2.2);}}}
    real td_r = get_real(td_complex);
    real td_i = get_imag(td_complex);
}
parameters {
    complex p_complex;
    complex p_complex_array[N];
    complex p_complex_array_2d[N, 2];
    complex p_complex_array_3d[N, 2, 3];
}
transformed parameters {
    complex tp_complex = p_complex;
    tp_complex = 1;
    tp_complex = to_complex(1,2);
    complex tp_complex_array[N] = p_complex_array;
    complex tp_complex_array_2d[2, 3];
    complex tp_complex_array_3d[2, 3, 4];
    for (tp_i in 1:2) {
        for (tp_j in 1:3) {
            for (tp_k in 1:4) {
                tp_complex_array_3d[tp_i, tp_j, tp_k] = to_complex(1, 2.2);}}}
    real tp_r = get_real(tp_complex);
    real tp_i = get_imag(tp_complex);
}
model {
   abs(p_complex) ~ normal(0, 1);
}
generated quantities {
    complex gq_complex;
    complex gq_complex_array[2];
    complex gq_complex_array_2d[2, 3] = {{1, 2, 3}, {to_complex(), to_complex(1.1), to_complex(1,2.1)}};
    complex gq_complex_array_3d[2, 3, 4];
    gq_complex = 3;
    gq_complex_array[1] = to_complex(5.1,6);

    gq_complex += 1;
    gq_complex += 3.1;
    gq_complex += 4i;
    print(gq_complex);
    gq_complex = add(1,3i);
    gq_complex = add(3i,1);
    gq_complex = add(3i,0.5);
    gq_complex = add(0.5,3i);

    gq_complex = foo(gq_complex);
    gq_complex = foo(1);
    gq_complex = foo(1.6);

    // test imaginary literal
    complex zi = 1+3.14i;
    zi = zi * 0i;
    complex yi = to_complex(0, 1.1) + to_complex(0.0, 2.2) + to_complex();
    real x = get_real(3i - 40e-3i);
}
