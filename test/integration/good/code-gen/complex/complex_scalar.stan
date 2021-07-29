functions {
    complex foo(){
        return to_complex();
    }
    real foo1(complex z){
        return 1.0;
    }
    complex foo2(real r){
        return to_complex(r);
    }
    complex foo3(complex z){
        return z;
    }
    complex[] foo4(){
        return {to_complex(), to_complex()};
    }
    real foo5(complex[] z){
        return 1.0;
    }
    complex[] foo6(real r){
        return {to_complex(r), to_complex(r,r)};
    }
    complex[] foo7(complex[] z){
        return z;
    }
}
data {
    complex d_complex;
    complex d_complex_array[2];
    complex d_complex_array_2d[2, 3];
}
transformed data {
    complex td_complex = d_complex;
    td_complex = 1;
    td_complex = to_complex(1,2);
    complex td_complex_array[2] = d_complex_array;
    complex td_complex_array_2d[2, 3];
    
    real td_r;

    td_r = get_real(td_complex);
    td_r = get_imag(td_complex);
    td_r = get_real(td_complex_array[1]);
    td_r = get_imag(td_complex_array[1]);

    for (td_i in 1:2) {
        for (td_j in 1:3) {
            td_complex_array_2d[td_i, td_j] = to_complex(1, 2.2);}}
    
}
parameters {
    complex p_complex;
    complex p_complex_array[2];
    complex p_complex_array_2d[2, 3];
}
transformed parameters {
    real tp_r;
    complex tp_complex;
    complex tp_complex_array[2];
    complex tp_complex_array_2d[2, 3];
    
    tp_complex = 1;
    tp_complex = 1.1;
    tp_complex = p_complex;
    tp_complex = p_complex_array[1];
    tp_complex = to_complex();
    tp_complex = to_complex(1);
    tp_complex = to_complex(1.2);
    tp_complex = to_complex(1, 2);
    tp_complex = to_complex(1.1, 2);
    tp_complex = to_complex(1, 2.2);
    tp_complex = to_complex(1.1, 2.2);

    tp_r = get_real(tp_complex);
    tp_r = get_imag(tp_complex);
    tp_r = get_real(tp_complex_array[1]);
    tp_r = get_imag(tp_complex_array[1]);

    for (tp_i in 1:2) {
        for (tp_j in 1:3) {
            tp_complex_array_2d[tp_i, tp_j] = to_complex(1, 2.2);}}
}
model {
    complex m_complex;
    complex m_complex_array[2];
    complex m_complex_array_2d[2, 3];

    abs(p_complex) ~ normal(0, 1);
}
generated quantities {
    int gq_i = 1;
    real gq_r = 1.1;
    complex gq_complex;
    complex gq_complex_array[2];
    complex gq_complex_array_2d[2, 3];

    gq_complex = gq_i;
    gq_complex = gq_r;
    gq_complex = 1;
    gq_complex = 1.1;
    gq_complex = d_complex;
    gq_complex = d_complex_array[1];
    gq_complex = to_complex();
    gq_complex = to_complex(1);
    gq_complex = to_complex(1.2);
    gq_complex = to_complex(1, 2);
    gq_complex = to_complex(1.1, 2);
    gq_complex = to_complex(1, 2.2);
    gq_complex = to_complex(1.1, 2.2);

    gq_complex_array = d_complex_array;
    gq_complex_array = {gq_complex, 1, to_complex(2,3)};
    gq_complex_array[1] = to_complex(5.1,6);
    
    gq_complex_array_2d = d_complex_array_2d;
    gq_complex_array_2d = {{1, gq_complex, 3}, {to_complex(), to_complex(1.1), to_complex(1,2.1)}};
    gq_complex_array_2d[1,1] = to_complex(1,2);
    gq_complex_array_2d[1] = gq_complex_array;
    gq_complex_array_2d[1] = {gq_complex, to_complex(1,2), 2.4};
    
    gq_r = get_real(gq_complex);
    gq_r = get_imag(gq_complex);
    gq_r = get_real(gq_complex_array[1]);
    gq_r = get_imag(gq_complex_array[1]);
    

    gq_complex = foo();
    gq_r = foo1(gq_complex);
    gq_complex = foo2(gq_r);
    gq_complex = foo3(gq_complex);
    gq_complex_array = foo4();
    gq_r = foo5(gq_complex_array);
    gq_complex_array = foo6(gq_r);
    gq_complex_array = foo7(gq_complex_array);
   
    gq_i = (gq_complex == gq_complex);
    gq_i = (gq_complex_array[1] == gq_complex_array[1]);
    gq_i = (gq_complex_array_2d[1,1] == gq_complex_array[1]);
    gq_i = (gq_complex_array_2d[1,1] == gq_complex_array_2d[1,1]);  
}
