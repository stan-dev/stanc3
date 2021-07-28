functions {
    complex foo(real r, real i){
        return to_complex(r, i);
    }
}
data {
    complex d_z;
    complex d_z_ar[5];
}
parameters {
    complex p_z;
    complex p_z_ar[5];
}
model {
    complex m_z = 1;
    abs(m_z) ~ normal(0, 1);
}
generated quantities {
    complex gq_z_1 = to_complex();
    complex gq_z_2 = foo(1, 2);

    real r_1 = get_real(gq_z_2);
    real i_1 = get_imag(gq_z_2);
   
    int i_2 = (to_complex(gq_z_1) == gq_z_1);
    
    int a = 1;
    real b = 1.1;
    gq_z_1 = a;
    gq_z_1 = b;

    complex gq_z_ar[5] = {gq_z_1, to_complex(1,2), a, 0, 2.4};
}
