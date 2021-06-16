data {
    int N;
    real x;
    real y[N];
    vector[N] v;
}
transformed data {
    function
    real capture_data(real z, row_vector r) {
        real rs = sum(r);
        real ys = sum(y);
        real vs = sum(v);
        return z + rs + x + ys + vs;
    }
}
parameters {
    real p;
    real pa[N];
    vector[N] pv;
}
model {
    function
    real capture_data_and_params(real z, row_vector r) {
        real rs = sum(r);
        real ys = sum(y);
        real vs = sum(v);
        real pas = sum(pa);
        real pvs = sum(pv);
        return z + rs + x + ys + vs + p + pas + pvs;
    }
    target += capture_data(0.0,[1.0]);
    target += capture_data(p,[1.0]);
    target += capture_data_and_params(0.0,[1.0]);
    target += capture_data_and_params(p,[1.0]);
}