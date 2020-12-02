data {
    int N;
    vector[N] d_vector;
}

parameters {
    vector[N] p1_vector_varmat;
    vector[N] p2_vector_varmat;
}
model {
    target += normal_lpdf(d_vector| p1_vector_varmat, p2_vector_varmat);
}