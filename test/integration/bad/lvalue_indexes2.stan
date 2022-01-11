data {
    vector[5] x;
}
transformed data {
    vector[5] y;
    y[:][:] = x[:][:];
}